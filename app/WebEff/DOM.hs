module WebEff.DOM
  ( DOM, evalDOM

  , Html(..)
  , Attribute(..)
  , AttributeValue(..)

  , ElementName
  , AttributeName


  , AttrValueConstraints
  , HasSetAttributeValue(..)
  , (=:)


  , consoleLog

  , renderWith
  , NodeRef


  , module WebEff.DOM.Elements
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Effectful
import           WebEff.DOM.Elements
import           WebEff.DOM.FFI
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..))
import           WebEff.DOM.Tree
import           WebEff.DOM.Types

--------------------------------------------------------------------------------


-- data Html a action = TextNode a !Text
--                    | Node { tag        :: !ElementName
--                           , nodeVal    :: a
--                           , attributes :: Map.Map AttributeName (AttributeValue action)
--                           , children   :: Seq.Seq (Html a action)
--                           }
--                    deriving (Eq,Functor)

-- type NodeRef = FFI.Node

-- -- type NodeData action = (ElementName, Map.Map AttributeName (AttributeValue action))



-- -- -- | Allocates all nodes in the tree (and annote the tree with the refs)
-- -- allocate        :: DOM :> es => Html a action -> Eff es (Html NodeRef action)
-- -- allocate = \case
-- --   TextNode _ t       -> do node <- createTextNode t
-- --                            pure $ TextNode node t
-- --   Node tag _ ats chs -> do node <- createElement tag
-- --                            chs' <- traverse (allocate node) chs
-- --                            pure $ Node tag node ats chs'


-- -- addToDom root = traverse



-- -- data Tree a b = Leaf a
-- --               | Node



-- -- renderNode               :: NodeData action -> NodeRef -> Eff es ()
-- -- renderNode (tag,ats) ref = do


-- -- renderWith :: NodeRef -- ^ parent
-- --            -> Html (First NodeRef) action
-- --            -> Eff es (Html (First NodeRef) action)
-- -- renderWith parent = \case
-- --   TextNode

-- -- topDown             :: (a -> Text -> f b)
-- --                     -> (ElementName -> a -> Map.Map AttributeName (AttributeValue action) -> f b)
-- --                     -> Html a action -> f (Html b action)
-- -- topDown fText fNode = go
-- --   where
-- --     go = undefined

-- -- -- allocate =

-- -- allocate root tree = topDown (\_ t -> \parent -> )
-- --                              ()
-- --                              tree
-- --                              root
-- --   where
-- --     fText :: a -> Text -> NodeRef -> Eff es NodeRef
-- --     fText _ t parent = do node <- createTextNode t
-- --                           appendChild parent node
-- --                           pure node
-- --     fNode :: ElementName
-- --           -> a -> Map.Map AttributeName (AttributeValue action) -> NodeRef -> Eff es NodeRef
-- --     fNode tag _ ats parent = do node <- createElement tag
-- --                                 appendChild parent node
-- --                                 applyAttrs ats node
-- --                                 pure node

-- -- applyAttrs :: Map.Map AttributeName (AttributeValue action) -> NodeRef ->

-- -- allocate        :: NodeRef -> Html a action -> Html NodeRef action
-- -- allocate parent = \case
-- --   TextNode _ t ->

-- instance Bifunctor Html where
--   bimap f g = go
--     where
--       go = \case
--         TextNode x t       -> TextNode (f x) t
--         Node tag x ats chs -> Node tag (f x) (fmap g <$> ats) (go <$> chs)

-- instance Bifoldable Html where
--   bifoldMap f g = go
--     where
--       go = \case
--         TextNode x t       -> f x
--         Node tag x ats chs -> f x <> foldMap (foldMap g) ats <> foldMap go chs

-- instance Bitraversable Html where
--   bitraverse f g = go
--     where
--       go = \case
--         TextNode x t       -> flip TextNode t <$> f x
--         Node tag x ats chs -> Node tag <$> f x <*> traverse (traverse g) ats <*> traverse go chs
