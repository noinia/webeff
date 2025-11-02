module WebEff.Signal.Derived
  ( DerivedSignal(..)
  ) where

import WebEff.Reactive
import Data.Typeable

--------------------------------------------------------------------------------

-- | A derived Signal
data DerivedSignal t b where
  Derive :: Typeable a => Signal t a -> (a -> b) -> DerivedSignal t b

instance Functor (DerivedSignal t) where
  fmap f (Derive signal g) = Derive signal (f . g)

instance HasCurrent DerivedSignal a where
  current ctx (Derive signal f) = f <$> getSignal ctx signal
