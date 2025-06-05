module WebEff.DOM.Tree
  ( Html(..,TextNode,Node)
  , TextData(..)
  , ElemData(..)
  , Tree(..)

  , renderWith
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Effectful
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..), IsNode(..))
import           WebEff.DOM.Types

--------------------------------------------------------------------------------

-- | Rose tree with nodes of type 'n' and leaves of type 'l'
data Tree n l = Leaf l
              | Branch n (Seq.Seq (Tree n l))
              deriving (Show,Eq,Functor,Foldable,Traversable)

instance Bifunctor Tree where
  bimap f g = go
    where
      go = \case
        Leaf x       -> Leaf   (g x)
        Branch x chs -> Branch (f x) (fmap go chs)

instance Bifoldable Tree where
  bifoldMap f g = go
    where
      go = \case
        Leaf x       -> g x
        Branch x chs -> f x <> foldMap go chs

instance Bitraversable Tree where
  bitraverse f g = go
    where
      go = \case
        Leaf x       -> Leaf   <$> g x
        Branch x chs -> Branch <$> f x <*> traverse go chs


--------------------------------------------------------------------------------

data TextData l = TextData Text l
                deriving (Show,Eq,Ord,Functor,Foldable,Traversable)


allocateTextData (TextData t _) = TextData t <$> createTextNode t


data ElemData msg n = ElemData !ElementName
                                  n
                                  (Map.Map AttributeName (AttributeValue msg))
                       deriving (Functor,Foldable,Traversable)
-- AttributeName is incorrect; it may also be an event.


instance Bifunctor ElemData where
  bimap f g (ElemData tag x attrs) = ElemData tag (g x) (fmap (fmap f) attrs)

instance Bifoldable ElemData where
  bifoldMap f g (ElemData tag x attrs) = g x <> foldMap (foldMap f) attrs

instance Bitraversable ElemData where
  bitraverse f g (ElemData tag x attrs) = ElemData tag <$> g x <*> traverse (traverse f) attrs



allocateElemData :: DOM :> es => ElemData msg a -> Eff es (ElemData msg NodeRef)
allocateElemData (ElemData tag _ attrs) = (\node -> ElemData tag node attrs) <$> createElement tag


-- renderElem parent (ElemData tag _ attrs) = do node <-
--                                               appendChild parent node
--                                               Map.traverseWithKey (applyAttrs node) attrs
--                                               pure $ ElemData tag node attrs




newtype Html a msg = Html (Tree (ElemData msg a) (TextData a))

pattern TextNode t x = Html (Leaf (TextData t x))
pattern Node tag x attrs chs = Html (Branch (ElemData tag x attrs) chs)


instance Functor (Html a) where
  fmap = fmapDefault
instance Foldable (Html a) where
  foldMap = foldMapDefault
instance Traversable (Html a) where
  traverse f = secondA f

instance Bifunctor Html where
  bimap f g (Html tree) = Html $ bimap (bimap g f) (fmap f) tree
    -- see bitraverse

instance Bifoldable Html where
  bifoldMap f g (Html tree) = bifoldMap (bifoldMap g f) (foldMap f) tree

instance Bitraversable Html where
  bitraverse :: forall f a b msg msg'. Applicative f
             => (a -> f b) -> (msg -> f msg') -> Html a msg -> f (Html b msg')
  bitraverse f g (Html tree) = Html <$> bitraverse travBranch travLeaf tree
    where
      travBranch :: ElemData msg a -> f (ElemData msg' b)
      travBranch = bitraverse g f

      travLeaf :: TextData a -> f (TextData b)
      travLeaf = traverse f


-- | Create NodeRefs for all values.
allocate             :: DOM :> es => Html a msg -> Eff es (Html NodeRef msg)
allocate (Html tree) = Html <$> bitraverse allocateElemData allocateTextData tree


firstA_ f = bitraverse_ f (const $ pure ())


applyAttributes             :: ( DOM :> es
                               ) => Html NodeRef msg -> Eff es ()
applyAttributes (Html tree) = firstA_ (\(ElemData _ nodeRef ats) ->
                                Map.traverseWithKey (setAttribute' nodeRef) ats) tree
  where
    setAttribute' node attrName = \case
      Message msg   -> pure () -- addEventListener node attrName (handler msg)
                    -- TODO; I guess this should start some listener
      AttrValue val -> setAttribute node attrName val

-- | Add the tree into the DOM at the given location
addToDOM                  :: forall root es msg. (DOM :> es, IsNode root)
                          => root -> Html NodeRef msg -> Eff es ()
addToDOM root (Html tree) = go root tree
  where
    go        :: IsNode parent => parent -> Tree (ElemData msg NodeRef) (TextData NodeRef)
              -> Eff es ()
    go parent = \case
      Leaf textNode       -> traverse_ (appendChild parent) textNode
      Branch elemNode chs -> traverse_ (\node -> do appendChild parent node
                                                    traverse_ (go node) chs
                                       ) elemNode


renderWith           :: (DOM :> es, IsNode root)
                     => root -> Html a msg -> Eff es (Html NodeRef msg)
renderWith root html = do html' <- allocate html
                          applyAttributes html'
                          addToDOM root html'
                          pure html'


-- allocate =
