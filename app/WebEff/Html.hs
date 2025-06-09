{-# LANGUAGE OverloadedStrings #-}
module WebEff.Html
  ( -- * Generic element constructors
    textNode
  , el
  -- * Html elements
  , doctype
  , html
  , head
  , meta
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , div
  , p
  , hr
  , pre
  , blockquote
  , code
  , em
  , span
  , a
  , strong
  , i
  , b
  , u
  , sub
  , sup
  , br
  , ol
  , ul
  , li
  -- , liKeyed
  , dl
  , dt
  , dd
  , img
  , iframe
  , canvas
  , math
  , script
  , link
  , style_
  , select
  , option
  , textarea
  , form
  , input
  , button
  , section
  , header
  , footer
  , nav
  , article
  , aside
  , address
  , main
  , body
  , figure
  , figcaption
  , table
  , caption
  , colgroup
  , col
  , tbody
  , thead
  , tfoot
  , tr
  -- , trKeyed
  , td
  , th
  , label
  , fieldset
  , legend
  , datalist
  , optgroup
  , keygen
  , output
  , progress
  , meter
  , center
  , mark
  , ruby
  , rt
  , rp
  , bdi
  , bdo
  , wbr
  , audio
  , video
  , source
  , track
  , embed
  , object
  , param
  , ins
  , del
  , small
  , cite
  , dfn
  , abbr
  , time
  , var
  , samp
  , kbd
  , q
  , s
  , details
  , summary
  , menuitem
  , menu
  ) where


import           Data.Coerce
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (div, head, span)
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..))
import           WebEff.DOM.Tree

--------------------------------------------------------------------------------

textNode :: Default a => Text -> Html es a msg
textNode = flip TextNode def

-- | Constructs an Element with the given name, attributes, and children.
el                :: Default a
                  => ElementName -> [Attribute es msg] -> [Html es a msg] -> Html es a msg
el elName ats chs = Node elName def ats' evts' (Seq.fromList $ coerce chs)
  where
    (ats', evts') = flip foldMap ats $ \case
      EventAttr eventName msg -> (mempty, Map.singleton eventName msg)
      Attr      attrName val  -> (Map.singleton attrName val, mempty)


-- | Construct an element (internal function that avoids having to wrap the AttributeName)
el'        :: Default a => Text -> [Attribute es msg] -> [Html es a msg] -> Html es a msg
el' elName = el (ElementName elName)


--------------------------------------------------------------------------------
-- Converting these from miso

-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
div  = el' "div"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
table  = el' "table"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
thead  = el' "thead"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
tbody  = el' "tbody"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
tr  = el' "tr"
-----------------------------------------------------------------------------
-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>
-----------------------------------------------------------------------------
-- trKeyed :: Key -> Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
-- trKeyed = node HTML "tr" . pure
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
th  = el' "th"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
td  = el' "td"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
tfoot  = el' "tfoot"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
section  = el' "section"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
header  = el' "header"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
footer  = el' "footer"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
button = el' "button"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
form = el' "form"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
p = el' "p"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
s = el' "s"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
ul = el' "ul"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
span = el' "span"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
strong = el' "strong"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
li = el' "li"
-----------------------------------------------------------------------------
-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
-- liKeyed :: Key -> Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
-- liKeyed = node HTML "li" . pure
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h1 = el' "h1"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h2 = el' "h2"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h3 = el' "h3"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h4 = el' "h4"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h5 = el' "h5"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
h6 = el' "h6"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: Default a => [Attribute es msg] -> Html es a msg
hr = flip (el' "hr") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
pre = el' "pre"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: Default a => [Attribute es msg] -> Html es a msg
input = flip (el' "input") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
label = el' "label"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
a = el' "a"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
mark = el' "mark"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
ruby = el' "ruby"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
rt = el' "rt"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
rp = el' "rp"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
bdi = el' "bdi"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
bdo = el' "bdo"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: Default a => [Attribute es msg] -> Html es a msg
wbr = flip (el' "wbr") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
details = el' "details"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
summary = el' "summary"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
menuitem = el' "menuitem"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
menu = el' "menu"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
fieldset = el' "fieldset"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
legend = el' "legend"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
datalist = el' "datalist"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
optgroup = el' "optgroup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
keygen = el' "keygen"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
output = el' "output"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
progress = el' "progress"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
meter = el' "meter"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
center = el' "center"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
audio = el' "audio"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
video = el' "video"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: Default a => [Attribute es msg] -> Html es a msg
source = flip (el' "source") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: Default a => [Attribute es msg] -> Html es a msg
track = flip (el' "track") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: Default a => [Attribute es msg] -> Html es a msg
embed = flip (el' "embed") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
object = el' "object"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: Default a => [Attribute es msg] -> Html es a msg
param = flip (el' "param") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
ins = el' "ins"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
del = el' "del"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
small = el' "small"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
cite = el' "cite"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
dfn = el' "dfn"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
abbr = el' "abbr"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
time = el' "time"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
var = el' "var"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
samp = el' "samp"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
kbd = el' "kbd"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
caption = el' "caption"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
colgroup = el' "colgroup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: Default a => [Attribute es msg] -> Html es a msg
col = flip (el' "col") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
nav = el' "nav"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
article = el' "article"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
aside = el' "aside"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
address = el' "address"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
main = el' "main"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
body = el' "body"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
figure = el' "figure"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
figcaption = el' "figcaption"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
dl = el' "dl"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
dt = el' "dt"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
dd = el' "dd"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: Default a => [Attribute es msg] -> Html es a msg
img = flip (el' "img") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
iframe = el' "iframe"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
canvas = el' "canvas"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
math = el' "math"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
select = el' "select"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
option = el' "option"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
textarea = el' "textarea"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
sub = el' "sub"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
sup = el' "sup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: Default a => [Attribute es msg] -> Html es a msg
br = flip (el' "br") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
ol = el' "ol"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
blockquote = el' "blockquote"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
code = el' "code"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
em = el' "em"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
i = el' "i"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
b = el' "b"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
u = el' "u"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
q = el' "q"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: Default a => [Attribute es msg] -> Html es a msg
link = flip (el' "link") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style
--
-- This takes the raw text to be put in the style tag.
--
-- That means that if any part of the text is not trusted there's
-- a potential CSS injection. Read more at
-- https://owasp.org/www-project-web-security-testing-guide/latest/4-Web_Application_Security_Testing/11-Client_Side_Testing/05-Testing_for_CSS_Injection
--
-- You can also easily shoot yourself in the foot with something like:
--
-- @'style_' [] "\</style\>"@
style_               :: Default a => [Attribute es msg] -> Text -> Html es a msg
style_ attrs rawText = el' "style" attrs [textNode rawText]

-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
--
-- This takes the raw text to be put in the script tag.
--
-- That means that if any part of the text is not trusted there's
-- a potential JavaScript injection. Read more at
-- https://owasp.org/www-community/attacks/xss/
--
-- You can also easily shoot yourself in the foot with something like:
--
-- @'script_' [] "\</script\>"@
script :: Default a => [Attribute es msg] -> Text -> Html es a msg
script attrs rawText = el' "script" attrs [textNode rawText]
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Glossary/Doctype
doctype :: Default a => Html es a msg
doctype = el' "doctype" [] []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html
html :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
html = el' "html"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head
head :: Default a => [Attribute es msg] -> [Html es a msg] -> Html es a msg
head = el' "head"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta
meta :: Default a => [Attribute es msg] -> Html es a msg
meta = flip (el' "meta") []
-----------------------------------------------------------------------------
