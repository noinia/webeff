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

text_ :: Default a => Text -> Html a action
text_ = flip TextNode def

el_                :: Default a
                   => ElementName -> [Attribute action] -> [Html a action] -> Html a action
el_ elName ats chs = Node elName def (Map.fromList ats) (Seq.fromList $ coerce chs)

-- | Construct an element (internal function that avoids having to wrap the AttributeName)
el'        :: Default a => Text -> [Attribute action] -> [Html a action] -> Html a action
el' elName = el_ (ElementName elName)

div_ :: Default a => [Attribute action] -> [Html a action] -> Html a action
div_ = el' "div"

p_ :: Default a => [Attribute action] -> [Html a action] -> Html a action
p_ = el' "p"
