{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module WebEff.DOM.Tree
  ( Html(..,TextNode,Node)
  , TextData(..)
  , ElemData(..)
  , Tree(..)

  , renderWith
  ) where

import           Control.Monad (void)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import           Data.Foldable.WithIndex
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Effectful
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName(..), IsNode(..), Event)
import           WebEff.Send
import           WebEff.Updated

import           Debug.Trace
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
data TextData l = TextData { textContent :: {-#UNPACK#-}!Text -- ^ The text we store
                           , textData    :: l
                           } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Allocates the textData element
allocateTextData                :: DOM :> es => TextData l -> Eff es (TextData NodeRef)
allocateTextData (TextData t _) = TextData t <$> createTextNode t

type Size = Int


data Patch es a = Patch Size (Eff es a)
                deriving (Functor)

instance Semigroup a => Semigroup (Patch es a) where
  (Patch sizeA a) <> (Patch sizeB b) = Patch (sizeA + sizeB) (a <> b)

patch' = Changed . Patch 1



-- | Computes the diff
diffTextData :: DOM :> es => TextData NodeRef -> TextData new -> Updated (Patch es ())
diffTextData (TextData old nodeRef) (TextData new _)
  | old == new = Unchanged
  | otherwise  = patch' (setTextContent nodeRef new)


-- | The Attributes of a Node
type Attributes = Map.Map AttributeName AttributeValue

-- | The event handlers that we store for each node
type EventHandlers es msg = Map.Map EventName (EventHandler es msg)

-- | Data for an actual element tag
data ElemData es msg n = ElemData { elemTag        :: {-#UNPACK#-}!ElementName
                                  , elemData       :: n
                                  , elemAttributes :: Attributes
                                  , elemEvents     :: EventHandlers es msg
                                  } deriving (Functor,Foldable,Traversable)

instance Bifunctor (ElemData es) where
  bimap f g (ElemData tag x ats evts) = ElemData tag (g x) ats (fmap (fmap f) evts)

-- instance Bifoldable ElemData where
--   bifoldMap f g (ElemData tag x _ evts) = g x <> foldMap f evts

-- instance Bitraversable ElemData where
--   bitraverse f g (ElemData tag x ats evts) =
--     (\x' evts' -> ElemData tag x' ats evts') <$> g x <*> traverse f evts


-- | Diff two existing attributes
diffAttr :: DOM :> es
         => NodeRef
         -> AttributeName -> AttributeValue -> AttributeValue
         -> Updated (Patch es ())
diffAttr node attr old new@(AttrValue new')
  | old == new = Unchanged
  | otherwise  = patch' (setAttribute node attr new')

-- | Diff the attributes of the given node. Produces a patch that we can apply to update
-- the DOM (if an update is required).
diffAttrs             :: forall es. DOM :> es => NodeRef -> Attributes -> Attributes
                      -> Updated (Patch es ())
diffAttrs node oldAts = fold . Map.merge removeOldAttrs setNewAttrs diffExisting oldAts
                        -- we produce a Map AttrName (Updated (Patch es)), and then fold
                        -- it into a big patch.
  where
    removeOldAttrs = Map.mapMissing $ \attr _old            -> patch' (removeAttribute node attr)
    setNewAttrs    = Map.mapMissing $ \attr (AttrValue new) -> patch' (setAttribute node attr new)
    diffExisting   :: Map.WhenMatched _
                      AttributeName AttributeValue AttributeValue (Updated (Patch es ()))
    diffExisting   = Map.zipWithMaybeMatched $ \attr old new -> case diffAttr node attr old new of
                                                                  Unchanged -> Nothing
                                                                  changed   -> Just changed

-- | Diff the Events of a particlar node
diffEvents              :: forall handlerEs es msg.
                           ( DOM :> es
                           , CanRunHandler handlerEs :> es
                           , Send msg                :> handlerEs
                           , DOM                     :> handlerEs
                           )
                        => NodeRef -> EventHandlers handlerEs msg -> EventHandlers handlerEs msg
                        -> Updated (Patch es ())
diffEvents node oldEvts = fold . Map.merge removeOldAttrs setNewAttrs diffExisting oldEvts
                        -- we produce a Map EventName (Updated (Patch es)), and then fold
                        -- it into a big patch.
  where
    removeOldAttrs = Map.mapMissing $ \evtName old             ->
                                        patch' (removeEventListeners node evtName old)

    setNewAttrs    = Map.mapMissing $ \evtName (EventHandler createMessage) ->
                                        patch' (addNewEvent evtName createMessage)

    diffExisting   :: Map.WhenMatched _ EventName (EventHandler handlerEs msg)
                                                  (EventHandler handlerEs msg)
                                        (Updated (Patch es ()))
    diffExisting   = Map.zipWithMaybeMatched $ \evt old new -> Nothing
      -- FIXME:: Assume they are the same for now
      -- case diffAttr node attr old new of
      --                                                             Unchanged -> Nothing
      --                                                             changed   -> Just changed

    addNewEvent evtName createMessage = void $
      addEventListener @handlerEs node evtName $ \event -> do msg <- createMessage event
                                                              sendMessage msg
    removeEventListeners _ _ _ = consoleLog "removing event listeners not yet implemented"



-- | Deiff an ElemDat
diffElemData :: forall handlerEs es msg new.
                ( DOM                     :> es
                , CanRunHandler handlerEs :> es
                , Send msg                :> handlerEs
                , DOM                     :> handlerEs
                )
             => ElemData handlerEs msg NodeRef
             -> ElemData handlerEs msg new
             -> Updated (Patch es (ElemData handlerEs msg NodeRef))
diffElemData old@(ElemData oldTag nodeRef oldAts oldEvts)
             new@(ElemData newTag _       newAts newEvts)
    | oldTag /= newTag = Changed $ Patch (elemSize old + elemSize new)
                                         (do new' <- allocateElemData new
                                             removeSelf nodeRef
                                             pure new'
                                         )
    | otherwise        = let patch :: Updated (Patch es ())
                             patch = diffAttrs             nodeRef oldAts  newAts
                                  <> diffEvents @handlerEs nodeRef oldEvts newEvts
                             -- the Patch to update the
                         in (new { elemData = nodeRef } <$)
                            <$> patch
                        -- combine the diffs/patches of the children and
                        -- plug in the current node

-- | Compute the size of a elem
elemSize    :: ElemData es msg a -> Int
elemSize el = 1 + Map.size (elemAttributes el) + Map.size (elemEvents el)



-- | Allocate an element
allocateElemData :: DOM :> es => ElemData handlerEs msg a -> Eff es (ElemData handlerEs msg NodeRef)
allocateElemData (ElemData tag _ ats evts) =
  (\node -> ElemData tag node ats evts) <$> createElement tag


-- renderElem parent (ElemData tag _ attrs) = do node <-
--                                               appendChild parent node
--                                               Map.traverseWithKey (applyAttrs node) attrs
--                                               pure $ ElemData tag node attrs



-- | Type representing Html trees each node storing an extra value of type a
newtype Html handlerEs a msg = Html (Tree (ElemData handlerEs msg a) (TextData a))

pattern TextNode t x = Html (Leaf (TextData t x))
pattern Node tag x ats evts chs = Html (Branch (ElemData tag x ats evts) chs)


instance Functor (Html handlerEs a) where
  fmap f (Html tree) = Html $ first (first f) tree

-- instance Foldable (Html a) where
--   foldMap = foldMapDefault
-- instance Traversable (Html a) where
--   traverse f = secondA f

instance Bifunctor (Html handlerEs) where
  bimap f g (Html tree) = Html $ bimap (bimap g f) (fmap f) tree
    -- see bitraverse

-- instance Bifoldable Html where
--   bifoldMap f g (Html tree) = bifoldMap (bifoldMap g f) (foldMap f) tree

-- instance Bitraversable Html where
--   bitraverse :: forall f a b msg msg'. Applicative f
--              => (a -> f b) -> (msg -> f msg') -> Html a msg -> f (Html b msg')
--   bitraverse f g (Html tree) = Html <$> bitraverse travBranch travLeaf tree
--     where
--       travBranch :: ElemData msg a -> f (ElemData msg' b)
--       travBranch = bitraverse g f

--       travLeaf :: TextData a -> f (TextData b)
--       travLeaf = traverse f


-- | Create NodeRefs for all values.
allocate             :: DOM :> es => Html handlerEs a msg -> Eff es (Html handlerEs NodeRef msg)
allocate (Html tree) = Html <$> bitraverse allocateElemData allocateTextData tree

-- | apply a function to the first of a bitraversable.
firstA_   :: (Applicative f, Bitraversable t) => (a -> f ()) -> t a c -> f ()
firstA_ f = bitraverse_ f (const $ pure ())

-- | Applies the given attributes
applyAttributes             :: forall handlerEs es msg.
                               ( DOM                     :> es
                               , CanRunHandler handlerEs :> es
                               , Send msg                :> handlerEs
                               , DOM                     :> handlerEs
                               ) => Html handlerEs NodeRef msg -> Eff es ()
applyAttributes (Html tree) = flip firstA_ tree $ \(ElemData _ nodeRef ats evts) -> do
  ifor_ ats $ \attrName (AttrValue val) ->
    setAttribute nodeRef attrName val
  ifor_ evts $ \evtName (EventHandler createMessage) -> void $
    addEventListener @handlerEs nodeRef evtName $ \event -> do msg <- createMessage event
                                                               sendMessage msg





-- | Add the tree into the DOM at the given location
addToDOM                  :: forall handlerEs root es msg. (DOM :> es, IsNode root)
                          => root -> Html handlerEs NodeRef msg -> Eff es ()
addToDOM root (Html tree) = go root tree
  where
    go        :: IsNode parent => parent -> Tree (ElemData handlerEs msg NodeRef) (TextData NodeRef)
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
                     => root -> Html handlerEs a msg -> Eff es (Html handlerEs NodeRef msg)
renderWith root html = do html' <- allocate html
                          addToDOM root html'
                          applyAttributes @handlerEs html'
                          pure html'
