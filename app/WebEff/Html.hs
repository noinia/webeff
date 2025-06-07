{-# LANGUAGE OverloadedStrings #-}
module WebEff.Html
  ( textNode
  , el
  , div
  , h1, h2, h3, h4, h5, h6
  , p
  , button
  ) where


import           Data.Coerce
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (div)
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..))
import           WebEff.DOM.Tree

--------------------------------------------------------------------------------

textNode :: Default a => Text -> Html a msg
textNode = flip TextNode def

-- | Constructs an Element with the given name, attributes, and children.
el                :: Default a
                  => ElementName -> [Attribute msg] -> [Html a msg] -> Html a msg
el elName ats chs = Node elName def ats' evts' (Seq.fromList $ coerce chs)
  where
    (ats', evts') = flip foldMap ats $ \case
      EventAttr eventName msg -> (mempty, Map.singleton eventName msg)
      Attr      attrName val  -> (Map.singleton attrName val, mempty)


-- | Construct an element (internal function that avoids having to wrap the AttributeName)
el'        :: Default a => Text -> [Attribute msg] -> [Html a msg] -> Html a msg
el' elName = el (ElementName elName)

div :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
div = el' "div"

h1 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h1 = el' "h1"

h2 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h2 = el' "h2"

h3 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h3 = el' "h3"

h4 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h4 = el' "h4"

h5 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h5 = el' "h5"

h6 :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
h6 = el' "h6"

p :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
p = el' "p"

button :: Default a => [Attribute msg] -> [Html a msg] -> Html a msg
button = el' "button"
