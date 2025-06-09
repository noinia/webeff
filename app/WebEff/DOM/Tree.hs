{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module WebEff.DOM.Tree
  ( Html(..,TextNode,Node)
  , TextData(..)
  , ElemData(..)
  , Tree(..)

  , renderWith
  , diffHtml


  , addToDOM

  , allocateTextData
  , allocateElemData

  , Patch
  , CanCombine
  , CanPatch(..)
  , spec
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
import           GHC.Show (showCommaSpace)
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName(..), IsNode(..), Event)
import           WebEff.Send
import           WebEff.Updated

import           Debug.Trace


-- import Test.Hspec
import           Effectful.Dispatch.Static


--------------------------------------------------------------------------------

spec = testLeaf

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

type Patch es = Eff es ()


class CanCombine t old where
  -- | given that old and new are matched. Update old into new
  combineMatching :: t old -> t new -> t old

class CanCombine t old => CanPatch t old es where
  -- | Compare old and new, creating a fine grained patch.
  patch :: t old -> t new -> Updated (Patch es, t old)

-- class CanReplace t old es where
--   -- | Replaces old by new (without any checking)
--   replace :: t old -> t new -> Updated (Patch es)


-- data Patch es a = Patch Size (Eff es a)
--                 deriving (Functor)

-- instance Semigroup a => Semigroup (Patch es a) where
--   (Patch sizeA a) <> (Patch sizeB b) = Patch (sizeA + sizeB) (a <> b)

-- patch' = Changed . Patch 1



--------------------------------------------------------------------------------

-- | Data for 'Text' elements
data TextData l = TextData { textContent :: {-#UNPACK#-}!Text -- ^ The text we store
                           , textData    :: l
                           } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Allocates the textData element
allocateTextData                :: DOM :> es => TextData l -> Eff es (TextData NodeRef)
allocateTextData (TextData t _) = TextData t <$> createTextNode t


-- | Computes the diff
diffTextData :: DOM :> es => TextData NodeRef -> TextData new -> Updated (Patch es)
diffTextData (TextData old nodeRef) (TextData new _)
  | old == new = Unchanged
  | otherwise  = Changed $ setTextContent nodeRef new

instance CanCombine TextData l where
  combineMatching (TextData _ node) (TextData new _) = TextData new node

instance DOM :> es => CanPatch TextData NodeRef es where
  patch old new = (,combineMatching old new) <$> diffTextData old new
  -- replace (TextData _ nodeRef) (TextData new _) = error "not implemented yet"


-- instance Show (Eff '[DOM] ()) where
--   show _ = "Eff"
instance Show NodeRef where
  show _ = "NodeRef"

testLeaf :: Spec
testLeaf = describe "leaf tests" $ do
             it "same" $
               patch @_ @_ @'[DOM] (TextData "foo" (undefined :: NodeRef)) (TextData "foo" ())
               `shouldSatisfy` (\case
                                   Unchanged -> True
                                   _         -> False
                               )
             it "different" $
               patch @_ @_ @'[DOM] (TextData "foo" (undefined :: NodeRef)) (TextData "bar" ())
               `shouldSatisfy` (\case
                                   Changed (_, TextData "bar" _) -> True
                                   _                             -> False
                               )

type Spec = IO ()
describe s act = do print s
                    act

it = describe

shouldSatisfy       :: a -> (a -> Bool) -> IO ()
x `shouldSatisfy` f = print $ f x




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


instance Show n => Show (ElemData es msg n) where
  showsPrec d (ElemData tag x ats evts)
    = showParen (d >= 11)
        ((.)
          (showString "ElemData {")
          ((.)
            (showString "elemTag = ")
                ((.)
                   (showsPrec 0 tag)
                   ((.)
                      showCommaSpace
                      ((.)
                         (showString "elemData = ")
                         ((.)
                            (showsPrec 0 x)
                            ((.)
                               showCommaSpace
                               ((.)
                                  (showString "elemAttributes = ")
                                  ((.)
                                     (showsPrec 0 ats)
                                     ((.)
                                        showCommaSpace
                                        ((.)
                                           (showString "elemEvents = ")
                                           ((.)
                                              (showsPrec 0 $ "EventHandler" <$ evts)
                                             (showString "}")))))))))))))




-- | Diff two existing attributes
diffAttr :: DOM :> es
         => NodeRef
         -> AttributeName -> AttributeValue -> AttributeValue
         -> Updated (Patch es)
diffAttr node attr old new@(AttrValue new')
  | old == new = Unchanged
  | otherwise  = Changed $ setAttribute node attr new'

-- | Diff the attributes of the given node. Produces a patch that we can apply to update
-- the DOM (if an update is required).
diffAttrs             :: forall es. DOM :> es => NodeRef -> Attributes -> Attributes
                      -> Updated (Patch es)
diffAttrs node oldAts = fold . Map.merge removeOldAttrs setNewAttrs diffExisting oldAts
                        -- we produce a Map AttrName (Updated (Patch es)), and then fold
                        -- it into a big patch.
  where
    removeOldAttrs = Map.mapMissing $ \attr _old            -> Changed (removeAttribute node attr)
    setNewAttrs    = Map.mapMissing $ \attr (AttrValue new) -> Changed (setAttribute node attr new)
    diffExisting   :: Map.WhenMatched _
                      AttributeName AttributeValue AttributeValue (Updated (Patch es))
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
                        -> Updated (Patch es)
diffEvents node oldEvts = fold . Map.merge removeOldAttrs setNewAttrs diffExisting oldEvts
                        -- we produce a Map EventName (Updated (Patch es)), and then fold
                        -- it into a big patch.
  where
    removeOldAttrs = Map.mapMissing $ \evtName old             ->
                                        Changed (removeEventListeners node evtName old)

    setNewAttrs    = Map.mapMissing $ \evtName (EventHandler createMessage) ->
                                        Changed (addNewEvent evtName createMessage)

    diffExisting   :: Map.WhenMatched _ EventName (EventHandler handlerEs msg)
                                                  (EventHandler handlerEs msg)
                                        (Updated (Patch es))
    diffExisting   = Map.zipWithMaybeMatched $ \evt old new -> Nothing
      -- FIXME:: Assume they are the same for now
      -- case diffAttr node attr old new of
      --                                                             Unchanged -> Nothing
      --                                                             changed   -> Just changed

    addNewEvent evtName createMessage = void $
      addEventListener @handlerEs node evtName $ \event -> do msg <- createMessage event
                                                              unsafeEff_ $ print "sending!"
                                                              sendMessage msg
                                                              unsafeEff_ $ print "done sending!"

    removeEventListeners _ _ _ = consoleLog "removing event listeners not yet implemented"


instance CanCombine (ElemData handlerEs msg) old where
  combineMatching old new = new { elemData = elemData old }

instance ( DOM                     :> es
         , CanRunHandler handlerEs :> es
         , Send msg                :> handlerEs
         , DOM                     :> handlerEs
         ) => CanPatch (ElemData handlerEs msg) NodeRef es where
  patch old new = (,combineMatching old new) <$> diffElemData old new

-- | Diff an ElemData
--
-- pre: Assumes the tags are the same
diffElemData :: forall handlerEs es msg new.
                ( DOM                     :> es
                , CanRunHandler handlerEs :> es
                , Send msg                :> handlerEs
                , DOM                     :> handlerEs
                )
             => ElemData handlerEs msg NodeRef -> ElemData handlerEs msg new
             -> Updated (Patch es)
diffElemData old@(ElemData _ node oldAts oldEvts) new@(ElemData _ _ newAts newEvts) =
     diffAttrs             node oldAts  newAts
  <> diffEvents @handlerEs node oldEvts newEvts
  -- combine the diffs/patches of the children and plug in the current node


replace = error "replace is called"


diffTree         :: forall a b old branch leaf es.
                    ( CanPatch branch old es
                    , CanPatch leaf   old es
                    )
                 => Tree (branch old) (leaf old) -> Tree (branch a) (leaf b)
                 -> Updated (Patch es, Tree (branch old) (leaf old))
diffTree oldTree = \case
  newTree@(Leaf new)          -> case oldTree of
    Leaf old          -> traceShow ("Diffing a leaf!") $ fmap Leaf <$> patch old new
    Branch _ _        -> replace oldTree newTree
  newTree@(Branch new newChs) -> case oldTree of
    Leaf _            -> replace oldTree newTree
    Branch old oldChs -> combineWith (\(patchNode, new')   -> (patchNode, Branch new' oldChs))
                                     (\(patchChs, newChs') -> (patchChs, Branch old newChs'))
                                     (\(patchNode, new') (patchChs, newChs') ->
                                        (patchNode <> patchChs, Branch new' newChs')
                                     )
                                     (patch old new)
                                     (mergeSeq diffTree oldChs newChs)

-- | Merge the old and new sequence; zipping the results
mergeSeq           :: (old -> new -> Updated (Patch es, old))
                   -> Seq.Seq old -> Seq.Seq new -> Updated (Patch es, Seq.Seq old)
mergeSeq f old new = let res  = Seq.zipWith (\oldE newE -> (oldE, f oldE newE)) old new
                         new' = fmap (\(oldE,mNewE) -> case mNewE of
                                         Unchanged         -> oldE
                                         Changed (_,newE') -> newE'
                                     ) res
                     in (,new') <$> foldMap (fmap fst . snd) res

-- replace :: Tree n l -> Tree n' l' -> Updated (Patch es)
-- replace =



-- diffTree (Leaf old) (Leaf new) = (Leaf new <$) <$> diffTextData old new
-- diffTree (Leaf old) new        = Changed $ Patch (treeSize new)
--                                                  (do let node = textData old
--                                                      replace node new
--                                                  )
-- diffTree (Node old chs) new@(Leaf _) = Changed $ Patch 1 (do let node = elemData old
--                                                              replace node new
--                                                          )
-- diffTree (Node old oldChs) newTree@(Node new newChs)
--     | elemTag old /= elemTag new = Changed $ Patch (elemSize old + elemSize new)
--                                                    (do let node = elemData old
--                                                        replace node newTree
--                                                    )
--     | otherwise                  = case diffElemData old new of
--         Unchanged     -> fmap (Node old) <$> diffChildren
--         Updated patch -> case diffChildren of
--                            Unchanged -> Updated patch

--           fmap (Node (new { elemData = elemData old }))
--                          <$>
--   where
--     diffChildren :: Updated (Patch _ (Seq.Seq (Tree _ _)))
--     diffChildren = zipSeqWith diffTree oldChs newChs

-- the types are note quite right yet


-- replace node new = undefined
                                                   -- replace

                                                   --   new' <- allocateElemData new
                                                   --   removeSelf nodeRef
                                                   --   pure new'


--                                  -- we need
-- zipSeqWith :: Seq.Seq a -> Seq.Seq b -> Seq.Seq
-- zipSeqWith = Seq.zipWith


-- | Diff an html tree
diffHtml :: forall handlerEs es msg new.
                ( DOM                     :> es
                , CanRunHandler handlerEs :> es
                , Send msg                :> handlerEs
                , DOM                     :> handlerEs
                )
             => Html handlerEs NodeRef msg -> Html handlerEs new msg
             -> Updated (Patch es, Html handlerEs NodeRef msg)
diffHtml (Html old) (Html new) = fmap Html <$> diffTree old new

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
  deriving Show

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
