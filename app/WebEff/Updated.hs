module WebEff.Updated
  ( Updated(..)
  ) where



-- | Type indicating whether the model has changed. This is essentially just a 'Maybe
-- model'.
data Updated model = Unchanged | Changed model
                   deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Semigroup model => Monoid (Updated model) where
  mempty = Unchanged

instance Semigroup model => Semigroup (Updated model) where
  Unchanged     <> r           = r
  (Changed l )  <> (Changed r) = Changed $ l <> r
  l             <> Unchanged   = l
