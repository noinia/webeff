{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module WebEff.Runtime
  ( Runtime
  , createRuntime


  , Signal
  , createSignal
  , getSignal
  , setSignal
  , modifySignal

  , RegisteredEffect
  , createEffect
  , runEffect
  , registerEffect
  ) where

import Data.Bifunctor
import Data.Coerce
import Control.Lens
import Data.EnumSet qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Data.Dynamic (Typeable)
import Effectful
import Effectful.State.Dynamic
--------------------------------------------------------------------------------



newtype Signal t a = Signal Int
                   deriving (Show,Eq,Ord,Enum)

newtype RegisteredEffect t = RegisteredEffect Int
                           deriving (Show,Eq,Ord,Enum)


data SignalData t = SignalData { _theValue    :: Dynamic.Dynamic
                               , _subscribers :: Set.EnumSet (RegisteredEffect t)
                               }

makeLenses ''SignalData

data Runtime t ls = Runtime { _rawSignals    :: IntMap (SignalData t)
                            , _nextSignalId  :: {-# UNPACK#-}!Int
                            , _rawEffects    :: IntMap (Eff ls ())
                            , _nextEffectId  :: {-# UNPACK#-}!Int
                            , _runner        :: forall a. Eff ls a -> IO a
                            , _currentEffect :: Maybe (RegisteredEffect t)
                            }

makeLenses ''Runtime

-- | Create a new runtime
createRuntime     :: (forall a. Eff ls a -> IO a) -> Runtime t ls
createRuntime run = Runtime IntMap.empty 0 IntMap.empty 0 run Nothing


--------------------------------------------------------------------------------

-- | Create a new Signal
createSignal    :: forall t ls es a.
                   (State (Runtime t ls) :> es, Typeable a) => a -> Eff es (Signal t a)
createSignal x0 = state $ \(runtime :: Runtime t ls) ->
                            let i     = runtime^.nextSignalId
                                sData = SignalData (Dynamic.toDyn x0) Set.empty
                            in ( Signal i :: Signal t a
                               , runtime&nextSignalId %~ succ
                                        &rawSignals   %~ IntMap.insert i sData
                               )


-- | Access the signal value
getSignal        :: forall t ls es a. (State (Runtime t ls) :> es
                                      , Typeable a
                                      ) => Signal t a -> Eff es a
getSignal signal = state $ \(runtime :: Runtime t ls) ->
    runtime&rawSignals.at (coerce signal) %%~ \case
      Nothing         -> error "getSignal. Absurd, signal not found !?"
      Just signalData -> Just <$> getAndSubscribe (runtime^.currentEffect) signalData
  where
    -- | Get the current value of the signal, furthermore register the currently running
    -- effect (if such an effect exists) as a subscriber of the signal.
    --
    -- returns the value, as well as the updated signal data (which
    -- contains the updated) subscribers.
    getAndSubscribe                    :: Maybe (RegisteredEffect t)
                                       -> SignalData t
                                       -> (a, SignalData t)
    getAndSubscribe current signalData =
      case Dynamic.fromDynamic (signalData^.theValue) of
        Nothing -> error $ "getSignal: absurd. wrong type at" <> show signal
        Just x  -> ( x
                   , case current of
                       Nothing  -> signalData
                       Just eff -> signalData&subscribers %~ Set.insert eff
                   )

-- | Set the signal to a given value. (Possibly registering the
-- current event as a subscriber). This
setSignal          :: forall t ls es a. ( State (Runtime t ls) :> es
                                        , Typeable a
                                        )
                   => Signal t a -> a -> Eff es a
setSignal signal x = modifySignal @t @ls (const x) signal


-- | Access and update the signal value. Returns the new value. This
-- triggers re-running the effects that subscribe to this signal
modifySignal          :: forall t ls es a. (State (Runtime t ls) :> es
                                         , Typeable a
                                         )
                      => (a -> a)
                       -- ^ update function
                      -> Signal t a -> Eff es a
modifySignal f signal = state $ \(runtime :: Runtime t ls) ->
    runtime&rawSignals.at (coerce signal) %%~ \case
      Nothing         -> error "modifySignal. Absurd, signal not found !?"
      Just signalData -> Just <$> getAndSubscribe (runtime^.currentEffect) signalData
  where
    -- | Get the current value of the signal, furthermore register the currently running
    -- effect (if such an effect exists) as a subscriber of the signal.
    --
    -- returns the value, as well as the updated signal data (which
    -- contains the updated) subscribers.
    getAndSubscribe                    :: Maybe (RegisteredEffect t)
                                       -> SignalData t
                                       -> (a, SignalData t)
    getAndSubscribe current signalData =
      case f <$> Dynamic.fromDynamic (signalData^.theValue) of
        Nothing -> error $ "modifySignal: absurd. wrong type at" <> show signal
        Just x  -> ( x
                   , signalData&theValue    .~ Dynamic.toDyn x
                               &subscribers %~ maybe id Set.insert current
                   )

-- TODO: we still need to notify the actual subscribers when we set the value.

--------------------------------------------------------------------------------

-- | Run some effectful computation using a local state.
withLocalState       :: (State s :> es)
                     => (s -> s)
                     -- ^ function to re-initialize the state
                     -> (s -> s -> s)
                     -- ^ function that recombines the previous state and the newer
                     -- state (i.e. after running the action) into a final state
                     -> Eff es a
                     -- ^ The effect to run with the local state
                     -> Eff es a
withLocalState initialize recombine act = do prevState <- state $ \s -> (s, initialize s)
                                             x         <- act
                                             modify $ recombine prevState
                                             pure x

-- | Create a new registered effect and run it.
createEffect     :: ( State (Runtime t ls) :> es
                    , Subset ls es
                    )
                 => Eff ls ()
                 -> Eff es (RegisteredEffect t)
createEffect eff = do
    effIx <- registerEffect eff
    runEffect effIx eff
    pure effIx

-- | Runs the local effect
runEffect           :: forall t ls es.
                       ( State (Runtime t ls) :> es
                       , Subset ls es
                       )
                    => RegisteredEffect t
                    -> Eff ls ()
                    -> Eff es ()
runEffect effIx eff = withLocalState (&currentEffect ?~ effIx)
                                     recombine  -- we restore only the currentEffect
                                     (inject eff)
  where
    recombine                    :: Runtime t ls -> Runtime t ls -> Runtime t ls
    recombine prevState newState = newState&currentEffect .~ (prevState^.currentEffect)

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
