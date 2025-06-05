{-# LANGUAGE OverloadedStrings #-}
module WebEff.DOM.Elements
  ( text_
  , el_
  , div_
  , p_
  ) where


import           Data.Coerce
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..))
import           WebEff.DOM.Tree
import           WebEff.DOM.Types

--------------------------------------------------------------------------------

text_ :: Default a => Text -> Html a msg
text_ = flip TextNode def

-- | Constructs an Element with the given name, attributes, and children.
el_                :: Default a
                   => ElementName -> [Attribute msg] -> [Html a msg] -> Html a msg
el_ elName ats chs = Node elName def ats' evts' (Seq.fromList $ coerce chs)
  where
    (ats', evts') = flip foldMap ats $ \case
      EventAttr eventName msg -> (mempty, Map.singleton eventName msg)
      Attr      attrName val  -> (Map.singleton attrName val, mempty)


-- | Construct an element (internal function that avoids having to wrap the AttributeName)
el'        :: Default a => Text -> [Attribute msg] -> [Html a msg] -> Html a msg
el' elName = el_ (ElementName elName)

div_ :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
div_ = el' "div"

p_ :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
p_ = el' "p"

button_ :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
button_ = el' "button"
