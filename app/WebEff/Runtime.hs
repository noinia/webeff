{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module WebEff.Runtime
  ( Runtime
  , createRuntime

  , createSignal
  , Signal

  , RegisteredEffect
  , createEffect
  , registerEffect
  ) where

import Control.Lens
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Data.Dynamic (Typeable)
import Effectful
import Effectful.State.Dynamic
--------------------------------------------------------------------------------


newtype Signal t a = Signal Int

newtype RegisteredEffect t = RegisteredEffect Int


data Runtime t ls = Runtime { _rawSignals   :: IntMap Dynamic.Dynamic
                            , _nextSignalId :: {-# UNPACK#-}!Int
                            , _rawEffects   :: IntMap (Eff ls ())
                            , _nextEffectId :: {-# UNPACK#-}!Int
                            , _runner       :: forall a. Eff ls a -> IO a
                            }

makeLenses ''Runtime

-- | Create a new runtime
createRuntime     :: (forall a. Eff ls a -> IO a) -> Runtime t ls
createRuntime run = Runtime IntMap.empty 0 IntMap.empty 0 run


-- | Create a new Signal
createSignal    :: forall t ls es a.
                   (State (Runtime t ls) :> es, Typeable a) => a -> Eff es (Signal t a)
createSignal x0 = state $ \(runtime :: Runtime t ls) ->
                            let i = runtime^.nextSignalId
                            in ( Signal i :: Signal t a
                               , runtime&nextSignalId %~ succ
                                        &rawSignals   %~ IntMap.insert i (Dynamic.toDyn x0)
                               )

-- | Create a new registered effect and run it.
createEffect   :: ( State (Runtime t ls) :> es
                  , Subset ls es
                  )
               => Eff ls ()
               -> Eff es (RegisteredEffect t)
createEffect f = do effIx <- registerEffect f
                    inject f -- TODO: this needs to do a bit more work I guess
                    pure effIx

-- | Create/register a new registered effect.
registerEffect   :: forall t ls es.
                    (State (Runtime t ls) :> es)
                 => Eff ls ()
                 -> Eff es (RegisteredEffect t)
registerEffect f = state $ \(runtime :: Runtime t ls) ->
                             let i = runtime^.nextEffectId
                             in ( RegisteredEffect i :: RegisteredEffect t
                                , runtime&nextEffectId %~ succ
                                         &rawEffects   %~ IntMap.insert i f
                                )
