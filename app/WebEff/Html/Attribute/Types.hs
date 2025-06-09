module WebEff.Html.Attribute.Types
  ( HtmlId(..)
  , CssClass(..)
  , CssStyle(..)
  ) where

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           WebEff.DOM.FFI.Classes (HasSetAttributeValue(..))

--------------------------------------------------------------------------------

-- | Render something as a Text
class HasTextRender a where
  renderAsText :: a -> Text

-- instance HasTextRender a => HasTextRender (Identity a) where
--   renderAsText (Identity x) = renderAsText x

instance HasTextRender Text where
  renderAsText = id

--------------------------------------------------------------------------------

newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)

--------------------------------------------------------------------------------

newtype CssClass = CssClass Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


-- data Style = Style

newtype CssStyle = CssStyle Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


--------------------------------------------------------------------------------
