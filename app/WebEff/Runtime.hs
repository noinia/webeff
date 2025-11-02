{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module WebEff.Runtime
  ( Runtime
  , currentEffect
  , nextSignalId, rawSignals
  , signalAt, signalAt'
  , signalAtDyn, signalAtDyn'
  , nextEffectId, rawEffects, effectAt

  , HasRuntime

  , withRuntime, withRuntime'
  , createRuntime


  , SignalData(SignalData), theValue, subscribers
  , signalValue

  , Signal(..)

  , RegisteredEffect(..)

  -- , createEffect'
  -- , runEffect'


  , Proxy
  , Ctx(..)
  ) where

import Data.Kind(Type)
import Data.Proxy
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Data.Coerce
import Control.Lens
import Data.EnumSet qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Data.Dynamic (Typeable)
import Effectful
import Data.Dynamic.Lens qualified as LensDynamic
import Data.Dynamic.Lens (_Dynamic)
import Effectful.State.Static.Shared

--------------------------------------------------------------------------------


newtype Signal t a = Signal Int
                   deriving (Show,Eq,Ord,Enum)

newtype RegisteredEffect t = RegisteredEffect Int
                           deriving (Show,Eq,Ord,Enum)


data SignalData t a = SignalData { _theValue    :: a
                                 , _subscribers :: Set.EnumSet (RegisteredEffect t)
                                 }
                    deriving (Functor,Foldable,Traversable)

makeLenses ''SignalData

-- | Access the value in the signalData
signalValue :: Typeable a => Traversal' (SignalData t Dynamic.Dynamic) a
signalValue = theValue._Dynamic

--------------------------------------------------------------------------------

data Runtime ls t = Runtime { _rawSignals    :: IntMap (SignalData t Dynamic.Dynamic)
                            , _nextSignalId  :: {-# UNPACK#-}!Int
                            , _rawEffects    :: IntMap (Eff (State (Runtime ls t) : ls) ())
                            , _nextEffectId  :: {-# UNPACK#-}!Int
                            , _currentEffect :: Maybe (RegisteredEffect t)
                            }

makeLenses ''Runtime


type HasRuntime ls t = State (Runtime ls t)

--------------------------------------------------------------------------------


-- | Access the signal value at the given signal
signalAt        :: forall ls t a. Typeable a
                => Signal t a -> Traversal' (Runtime ls t) (SignalData t a)
signalAt signal = signalAtDyn signal . wrap
  where
    wrap   :: (Applicative f, Typeable a)
           => (SignalData t a -> f (SignalData t a))
           -> SignalData t Dynamic.Dynamic -> f (SignalData t Dynamic.Dynamic)
    wrap f = fmap (fmap Dynamic.toDyn) . f . fmap fromDynamic'

    fromDynamic' :: Dynamic.Dynamic -> a
    fromDynamic' = fromMaybe (error "signalAt. wrong type?") . Dynamic.fromDynamic

-- | Access the signalData for a given signal. This gives accessto the Signal Data
-- as a Dynamic.
signalAtDyn        :: Signal t a -> Traversal' (Runtime ls t) (SignalData t Dynamic.Dynamic)
signalAtDyn signal = rawSignals.at (coerce signal)._Just

-- | Access the signalData at a given signal
signalAt'        :: Typeable a => Signal t a -> Lens' (Runtime ls t) (SignalData t a)
signalAt' signal = singular (signalAt signal)

-- | Access the signalData for a given signal. This gives accessto the Signal Data
-- as a Dynamic.
signalAtDyn'        :: Signal t a -> Lens' (Runtime ls t) (SignalData t Dynamic.Dynamic)
signalAtDyn' signal = singular (signalAtDyn signal)


effectAt       :: RegisteredEffect t
               -> Traversal' (Runtime ls t) (Eff (State (Runtime ls t) : ls) ())
effectAt effIx = rawEffects.ix (coerce effIx)

--------------------------------------------------------------------------------

-- | Create a new runtime
-- createRuntime     :: forall ls t. (forall a. Eff ls a -> IO a) -> Runtime ls t
-- createRuntime run = Runtime IntMap.empty 0 IntMap.empty 0 run Nothing

createRuntime :: forall ls t. Runtime ls t
createRuntime = Runtime IntMap.empty 0 IntMap.empty 0 Nothing

-- | Create a new new runtime, and run a computation with it.
withRuntime   :: forall ls a.
                 (forall (t :: Type).
                   Ctx ls t -> Eff (HasRuntime ls t : ls) a
                 ) -> Eff ls a
withRuntime f = withRuntime' $ \(runtime :: Runtime ls t) ->
                                 evalState runtime $ f (Ctx @ls @t)

withRuntime'   :: forall ls r. (forall (t :: Type). Runtime ls t -> r) -> r
withRuntime' f = f @() $ createRuntime @ls @()


-- withRuntime     :: forall ls es a.
--                    (forall (t :: Type). Runtime ls t ->

--                     Eff (State (Runtime ls t) : es) a)
--                 -> Eff es a
-- withRuntime act = let initialRuntime :: Runtime ls (Proxy ())
--                       initialRuntime = createRuntime @ls
--                   in evalState initialRuntime (act @(Proxy ()))

-- withRuntime   :: (forall t. Runtime ls t -> Eff es a) -> eff es a
-- withRuntime f =
--   undefined

--------------------------------------------------------------------------------

-- | Context
data Ctx (ls :: [Effect]) (t :: Type) = Ctx
