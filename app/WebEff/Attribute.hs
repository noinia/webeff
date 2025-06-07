{-# LANGUAGE OverloadedStrings  #-}
module WebEff.Attribute
  ( module WebEff.DOM.Attribute
  , onClick_
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
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName, EventName(..))
import qualified WebEff.DOM.FFI.Types as FFI

--------------------------------------------------------------------------------

-- | An on-click event
onClick_ :: msg -> Attribute msg
onClick_ = EventAttr (EventName "onclick")
