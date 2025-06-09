{-# LANGUAGE OverloadedStrings  #-}
module WebEff.Html.Attribute
  ( module WebEff.DOM.Attribute
  , module WebEff.Html.Attribute.Types
  , class_
  , classes
  , id
  , title
  , hidden
  , lang
  , type_
  , value
  , defaultValue
  , checked
  , placeholder
  , selected
  , accept
  , acceptCharset
  , action
  , autocomplete
  , autofocus
  , autosave
  , disabled
  , enctype
  , formation
  , list
  , maxlength
  , minlength
  , method
  , multiple
  , name
  , novalidate
  , pattern_
  , readonly
  , required
  , size
  , for
  , ref
  , form
  , max
  , min
  , step
  , cols
  , rows
  , wrap
  , href
  , target
  , download
  , downloadAs
  , hreflang
  , media
  , ping
  , rel
  , ismap
  , usemap
  , shape
  , coords
  , src
  , height
  , width
  , alt
  , loading
  , autoplay
  , controls
  , loop
  , preload
  , poster
  , default_
  , kind
  , srclang
  , sandbox
  , seamless
  , srcdoc
  , reversed
  , start
  , align
  , colspan
  , rowspan
  , headers
  , scope
  , async
  , charset
  , content
  , defer
  , httpEquiv
  , language
  , scoped
  , data_
  , styleInline
  ) where

import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (id, min, max)
import           WebEff.DOM.Attribute
import           WebEff.Html.Attribute.Types


--------------------------------------------------------------------------------

-- | helper function to create an attribute
mkAttr          :: AttrValueConstraints val => Text -> val -> Attribute es msg
mkAttr attr val = Attr (AttributeName attr) (AttrValue val)

-- | Helper for creating a text property
textProp :: Text -> Text -> Attribute es msg
textProp = mkAttr

-- | Helper for creating a boolean property
boolProp :: Text -> Bool -> Attribute es msg
boolProp = mkAttr

--------------------------------------------------------------------------------
-- Converting these from Miso

-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Globalattributes/title>
title :: Text -> Attribute es msg
title = textProp "title"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#selected>
selected ::  Bool -> Attribute es msg
selected = boolProp "selected"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden>
hidden ::  Bool -> Attribute es msg
hidden             = boolProp "hidden"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang>
lang :: Text -> Attribute es msg
lang             = textProp "lang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/value>
value :: Text -> Attribute es msg
value             = textProp "value"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/defaultValue>
defaultValue :: Text -> Attribute es msg
defaultValue      = textProp "defaultValue"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept>
accept :: Text -> Attribute es msg
accept            = textProp "accept"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/acceptCharset>
acceptCharset :: Text -> Attribute es msg
acceptCharset     = textProp "acceptCharset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/action>
action :: Text -> Attribute es msg
action            = textProp "action"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autocomplete>
autocomplete ::  Bool -> Attribute es msg
autocomplete b = textProp "autocomplete" (if b then "on" else "off")
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autosave>
autosave :: Text -> Attribute es msg
autosave          = textProp "autosave"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled>
disabled ::  Bool -> Attribute es msg
disabled          = boolProp "disabled"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/enctype>
enctype :: Text -> Attribute es msg
enctype           = textProp "enctype"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/formation>
formation :: Text -> Attribute es msg
formation         = textProp "formation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/list>
list :: Text -> Attribute es msg
list              = textProp "list"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/maxlength>
maxlength :: Text -> Attribute es msg
maxlength         = textProp "maxlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/minlength>
minlength :: Text -> Attribute es msg
minlength         = textProp "minlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/method>
method :: Text -> Attribute es msg
method            = textProp "method"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/multiple>
multiple ::  Bool -> Attribute es msg
multiple          = boolProp "multiple"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/novalidate>
novalidate ::  Bool -> Attribute es msg
novalidate        = boolProp "noValidate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/pattern>
pattern_ :: Text -> Attribute es msg
pattern_           = textProp "pattern"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/readonly>
readonly ::  Bool -> Attribute es msg
readonly          = boolProp "readOnly"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/required>
required ::  Bool -> Attribute es msg
required          = boolProp "required"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/size>
size :: Text -> Attribute es msg
size              = textProp "size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/for>
for :: Text -> Attribute es msg
for               = textProp "for"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ref>
ref :: Text -> Attribute es msg
ref               = textProp "ref"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/form>
form :: Text -> Attribute es msg
form               = textProp "form"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/max>
max :: Text -> Attribute es msg
max               = textProp "max"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/min>
min :: Text -> Attribute es msg
min               = textProp "min"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/step>
step :: Text -> Attribute es msg
step              = textProp "step"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/cols>
cols :: Text -> Attribute es msg
cols              = textProp "cols"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rows>
rows :: Text -> Attribute es msg
rows              = textProp "rows"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/wrap>
wrap :: Text -> Attribute es msg
wrap              = textProp "wrap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/target>
target :: Text -> Attribute es msg
target            = textProp "target"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/download>
download :: Text -> Attribute es msg
download          = textProp "download"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/downloadAs>
downloadAs :: Text -> Attribute es msg
downloadAs        = textProp "downloadAs"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hreflang>
hreflang :: Text -> Attribute es msg
hreflang          = textProp "hreflang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/media>
media :: Text -> Attribute es msg
media             = textProp "media"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ping>
ping :: Text -> Attribute es msg
ping              = textProp "ping"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rel>
rel :: Text -> Attribute es msg
rel               = textProp "rel"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ismap>
ismap :: Text -> Attribute es msg
ismap             = textProp "ismap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/usemap>
usemap :: Text -> Attribute es msg
usemap            = textProp "usemap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/shape>
shape :: Text -> Attribute es msg
shape             = textProp "shape"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/coords>
coords :: Text -> Attribute es msg
coords            = textProp "coords"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/src>
src :: Text -> Attribute es msg
src               = textProp "src"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/height>
height :: Text -> Attribute es msg
height            = textProp "height"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/width>
width :: Text -> Attribute es msg
width             = textProp "width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/alt>
alt :: Text -> Attribute es msg
alt               = textProp "alt"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/loading>
loading :: Text -> Attribute es msg
loading           = textProp "loading"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay>
autoplay ::  Bool -> Attribute es msg
autoplay          = boolProp "autoplay"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls>
controls ::  Bool -> Attribute es msg
controls          = boolProp "controls"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/loop>
loop ::  Bool -> Attribute es msg
loop              = boolProp "loop"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload>
preload :: Text -> Attribute es msg
preload           = textProp "preload"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/poster>
poster :: Text -> Attribute es msg
poster            = textProp "poster"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/default>
default_ ::  Bool -> Attribute es msg
default_           = boolProp "default"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/kind>
kind :: Text -> Attribute es msg
kind              = textProp "kind"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srclang>
srclang :: Text -> Attribute es msg
srclang           = textProp "srclang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/sandbox>
sandbox :: Text -> Attribute es msg
sandbox           = textProp "sandbox"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/seamless>
seamless :: Text -> Attribute es msg
seamless          = textProp "seamless"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srcdoc>
srcdoc :: Text -> Attribute es msg
srcdoc            = textProp "srcdoc"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/reversed>
reversed :: Text -> Attribute es msg
reversed          = textProp "reversed"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/start>
start :: Text -> Attribute es msg
start             = textProp "start"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/align>
align :: Text -> Attribute es msg
align             = textProp "align"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/colspan>
colspan :: Text -> Attribute es msg
colspan           = textProp "colspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rowspan>
rowspan :: Text -> Attribute es msg
rowspan           = textProp "rowspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/headers>
headers :: Text -> Attribute es msg
headers           = textProp "headers"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scope>
scope :: Text -> Attribute es msg
scope             = textProp "scope"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/async>
async :: Text -> Attribute es msg
async             = textProp "async"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/charset>
charset :: Text -> Attribute es msg
charset           = textProp "charset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/content>
content :: Text -> Attribute es msg
content           = textProp "content"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/defer>
defer :: Text -> Attribute es msg
defer             = textProp "defer"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/httpEquiv>
httpEquiv :: Text -> Attribute es msg
httpEquiv         = textProp "httpEquiv"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/language>
language :: Text -> Attribute es msg
language          = textProp "language"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scoped>
scoped :: Text -> Attribute es msg
scoped            = textProp "scoped"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/type>
type_ :: Text -> Attribute es msg
type_ = textProp "type"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/name>
name :: Text -> Attribute es msg
name = textProp "name"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/href>
href :: Text -> Attribute es msg
href = textProp "href"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id>
id :: HtmlId -> Attribute es msg
id = mkAttr "id"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/placeholder>
placeholder :: Text -> Attribute es msg
placeholder = textProp "placeholder"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/checked>
checked ::  Bool -> Attribute es msg
checked = boolProp "checked"
-----------------------------------------------------------------------------
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus>
autofocus ::  Bool -> Attribute es msg
autofocus = boolProp "autofocus"

-----------------------------------------------------------------------------
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ :: CssClass -> Attribute es msg
class_ = mkAttr "class"

-----------------------------------------------------------------------------
-- | Set "className" property with a list of given classes
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
classes :: Foldable f => f CssClass -> Attribute es msg
classes = class_ . combineClasses

-- | Renders classes
combineClasses :: Foldable f => f CssClass -> CssClass
combineClasses = CssClass . Text.unwords . map coerce . F.toList


-----------------------------------------------------------------------------
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_    :: Text -> Text -> Attribute es msg
data_ k v = textProp ("data-" <> k) v


-----------------------------------------------------------------------------
-- | Set "style" property
--
-- > view m = div [ styleInline "background-color:red;color:blue;" ] [ "foo" ]
--
-- https://developer.mozilla.org/en-US/docs/Web/CSS
styleInline :: Text -> Attribute es msg
styleInline = textProp "style"
-----------------------------------------------------------------------------
