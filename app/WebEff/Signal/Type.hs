module WebEff.Signal.Type
  ( Signal(..)
  ) where


--------------------------------------------------------------------------------

-- | A signal
newtype Signal t a = Signal Int
                   deriving (Show,Eq,Ord,Enum)
