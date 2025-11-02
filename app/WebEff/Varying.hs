{-# LANGUAGE UndecidableInstances #-}
module WebEff.Varying
  ( Varying
  , varying
  , constant
  ) where

import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Effectful
import WebEff.Reactive
import WebEff.Runtime (HasRuntime)
import Data.Dynamic qualified as Dynamic
import Data.Typeable
import Data.Coerce
import WebEff.Signal.Type

--------------------------------------------------------------------------------

type Signals      = IntSet.IntSet
type SignalValues = IntMap.IntMap Dynamic.Dynamic

--------------------------------------------------------------------------------

-- | A value that varies over time.
--
-- in FRP terminology; this would be a behavior
data Varying t b where
  Varying :: Signals -> (SignalValues -> b) -> Varying t b
    --the signals and signalValues should be the same

-- | Produce a Varying that does not change.
constant   :: a -> Varying t a
constant x = Varying IntSet.empty (const x)

-- | Lift a signal into a Varying
varying        :: Typeable a => Signal t a -> Varying t a
varying signal = Varying signals f
  where
    signals = IntSet.singleton (coerce signal)
    f signalValues =  case Dynamic.fromDynamic (signalValues IntMap.! (coerce signal)) of
      Nothing -> error "varying: wrong type!? "
      Just x  -> x

--------------------------------------------------------------------------------

instance Functor (Varying t) where
  fmap f (Varying signals g) = Varying signals (f . g)

instance Applicative (Varying t) where
  pure = constant
  -- ff :: Signals -> (a -> b)
  -- fx :: Signals -> a
  (Varying fSignals ff) <*> (Varying xSignals fx) = Varying signals f
    where
      signals        = fSignals `IntSet.union` xSignals
      f signalValues = ff signalValues (fx signalValues)

instance (HasRuntime ls t :> es) => HasCurrent ls t es Varying a where
  current ctx (Varying signals f) = f <$> sequence signalValues
    where
      signalValues = IntMap.fromSet (untypedGetSignalDyn ctx) signals
      -- we get thevalues from the sginals (as untyped dyns); making sure to register
      -- that we access those signal values.
