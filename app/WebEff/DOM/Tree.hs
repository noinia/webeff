{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Foldable.WithIndex
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Effectful
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName(..), IsNode(..), Event)
import           WebEff.Send


import Debug.Trace
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

-- | Data for 'Text' elements
data TextData l = TextData {-#UNPACK#-}!Text -- ^ The text we store
                           l -- ^ Sattelite data.
                deriving (Show,Eq,Ord,Functor,Foldable,Traversable)


allocateTextData (TextData t _) = TextData t <$> createTextNode t


-- | Data for an actual element tag
data ElemData msg n = ElemData {-#UNPACK#-}!ElementName
                                -- ^ The tag
                                n
                                -- ^ satelite data
                                (Map.Map AttributeName AttributeValue)
                                -- ^ The Attributes
                                (Map.Map EventName msg)
                                -- ^ The Events
                       deriving (Functor,Foldable,Traversable)

instance Bifunctor ElemData where
  bimap f g (ElemData tag x ats evts) = ElemData tag (g x) ats (fmap f evts)

instance Bifoldable ElemData where
  bifoldMap f g (ElemData tag x _ evts) = g x <> foldMap f evts

instance Bitraversable ElemData where
  bitraverse f g (ElemData tag x ats evts) =
    (\x' evts' -> ElemData tag x' ats evts') <$> g x <*> traverse f evts


-- | Allocate an element
allocateElemData :: DOM :> es => ElemData msg a -> Eff es (ElemData msg NodeRef)
allocateElemData (ElemData tag _ ats evts) =
  (\node -> ElemData tag node ats evts) <$> createElement tag


-- renderElem parent (ElemData tag _ attrs) = do node <-
--                                               appendChild parent node
--                                               Map.traverseWithKey (applyAttrs node) attrs
--                                               pure $ ElemData tag node attrs



-- | Type representing Html trees each node storing an extra value of type a
newtype Html a msg = Html (Tree (ElemData msg a) (TextData a))

pattern TextNode t x = Html (Leaf (TextData t x))
pattern Node tag x ats evts chs = Html (Branch (ElemData tag x ats evts) chs)


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


firstA_   :: (Applicative f, Bitraversable t) => (a -> f ()) -> t a c -> f ()
firstA_ f = bitraverse_ f (const $ pure ())

applyAttributes             :: forall handlerEs es msg.
                               ( DOM                     :> es
                               , CanRunHandler handlerEs :> es
                               , Send msg                :> handlerEs
                               , DOM                     :> handlerEs
                               ) => Html NodeRef msg -> Eff es ()
applyAttributes (Html tree) = flip firstA_ tree $ \(ElemData _ nodeRef ats evts) -> do
  ifor_ ats $ \attrName (AttrValue val) ->
    setAttribute nodeRef attrName val
  ifor_ evts $ \evtName msg -> do
    consoleLog ("XXXXXXXX" <> showT evtName)
    addEventListener @handlerEs nodeRef evtName $ \event -> do consoleLog "XXX"
                                                               parseEvent event
                                                               sendMessage msg



parseEvent   :: DOM :> handlerEs => Event -> Eff handlerEs ()
parseEvent _ = pure ()


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

-- | Render the tree in the given root element. This will allocate the entire tree as well
-- as add it to the tree to the dom.
renderWith           :: forall handlerEs es root msg a.
                        ( DOM                     :> es
                        , CanRunHandler handlerEs :> es
                        , Send msg                :> handlerEs
                        , DOM                     :> handlerEs
                        , IsNode root
                        )
                     => root -> Html a msg -> Eff es (Html NodeRef msg)
renderWith root html = do html' <- allocate html
                          addToDOM root html'
                          applyAttributes @handlerEs html'
                          pure html'


-- allocate =

showT :: Show a => a -> Text
showT = Text.pack . show
