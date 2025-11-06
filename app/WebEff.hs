{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
module WebEff(main) where


import Data.Foldable
import Data.Functor.Classes
import Control.Monad
import WebEff.Reactive
import WebEff.FFI
import WebEff.Varying
import WebEff.Signal.Derived
import WebEff.FFI.Types
import Data.Coerce
import Data.IntMap (IntMap)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Typeable
import Data.Dynamic qualified as Dynamic
import Effectful
import Effectful.State.Static.Shared
import Control.Lens
import WebEff.Runtime
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful.Dispatch.Static
import Data.Kind (Type)
import GHC.Wasm.Prim (JSVal)
import WebEff.DOM
-- import Data.Functor.Apply qualified as Apply

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------










  -- do modify $ \runTime ->
  --                            runTime {

  --                                    }


--------------------------------------------------------------------------------

type View t node = Html (Varying t) node

data Html f node = TextNode node (f Text)
                 | Element  ElementName node (f (Map.Map AttributeName (f Text         )))
                                             (f (Seq.Seq               (f (Html f node))))
            -- todo; it's abit weird that attributes are text
  deriving stock (Functor, Foldable, Traversable)


deriving instance (Show1 f, Show node) => Show (Html f node)
deriving instance (Eq1 f, Eq node)     => Eq   (Html f node)



-- | Map the f's to g's
bmap      :: forall f g node. Functor f => (forall a. f a -> g a) -> Html f node -> Html g node
bmap ftog = go
  where
    go = \case
      TextNode n text       -> TextNode n (ftog text)
      Element tag n ats chs -> Element tag n (ftog $ applyAts <$> ats) (ftog $ applyChs <$> chs)

    applyAts :: Map.Map AttributeName (f Text) -> Map.Map AttributeName (g Text)
    applyAts = fmap ftog

    applyChs :: Seq.Seq (f (Html f node)) -> Seq.Seq (g (Html g node))
    applyChs = fmap (ftog . fmap go)


-- I t hink this needs h to be a monad, moreover we need to pick whether to
-- traverse top down or bottom up

-- | Map the f's to g's
traverseF      :: forall f g h node. (Functor f, Monad h)
               => (forall a. f a -> h (g a)) -> Html f node -> h (Html g node)
traverseF ftog = undefined
  -- go
  -- where
  --   go = \case
  --     TextNode text       -> TextNode <$> ftog text
  --     Element tag ats chs -> Element tag <$> (flatten $ applyAts <$> ats)
  --                                        <*> (flatten $ applyChs <$> chs)

  --   flatten :: f (h a) -> h (g a)
  --   flatten = join . sequenceA . ftog
  --                -- h (g (h a))

  --   applyAts :: Map.Map AttributeName (f Text) -> h (Map.Map AttributeName (g Text))
  --   applyAts = traverse ftog

  --   applyChs :: Seq.Seq (f (Html f)) -> h (Seq.Seq (g (Html g)))
  --   applyChs = traverse (flatten . traverse go)


-- | Constructs a text node
textNode_ :: Applicative f => Text -> Html f ()
textNode_ = TextNode () . pure

-- | Constructs an element with fixed children (but each child itself
-- is properly wrapped in an f)
el_             :: forall f. Applicative f
                => ElementName
                -- ^ The element we are constructing
                -> [f (AttributeName, f Text)]
                -- ^ The Attributes. The outer f may be used to adapt
                -- each individual attribute.
                -> [f (Html f ())]
                -- ^ Children
                -> Html f ()
el_ tag ats chs = Element tag () res (pure $ Seq.fromList chs)
  where
    ats' :: f [Map.Map AttributeName (f Text)]
    ats' = traverse (fmap (uncurry Map.singleton)) ats

    res = fmap fold ats'

merakibtn = "px-6 py-2 font-medium tracking-wide text-white capitalize transition-colors duration-300 transform bg-blue-600 rounded-lg hover:bg-blue-500 focus:outline-none focus:ring focus:ring-blue-300 focus:ring-opacity-80"

myHtml :: Html (Constant t) ()
myHtml = div_ []
              [ pure $ button_ [ pure (id_ $ pure "minButton")
                               , pure (classes_ $ pure [ merakibtn
                                                       ]
                                      )
                               ]
                               [ pure $ textNode_ "-" ]
              , pure $ textNode_ "woei"
              , pure $ button_ [ pure (classes_ $ pure [merakibtn])
                               ]
                               [pure $ textNode_ "+"]
              ]

-- | Renders clasess
classes_ :: (Functor f, Foldable list) => f (list Text) -> (AttributeName, f Text)
classes_ = class_ . fmap (Text.unwords . toList)

class_   :: f Text -> (AttributeName, f Text)
class_ v = (AttributeName "class", v)

id_     :: f Text  -> (AttributeName, f Text)
id_ v   = (AttributeName "id", v)


div_ :: Applicative f => [f (AttributeName, f Text)] -> [f (Html f ())] -> Html f ()
div_ = el_ (ElementName "div")

button_ :: Applicative f => [f (AttributeName, f Text)] -> [f (Html f ())] -> Html f ()
button_ = el_ (ElementName "button")


-- | renders the given Html tree
construct      :: (IsNode parent, DOM :> es)
               => parent -> Html (Constant t) a -> Eff es (Html (Constant t) Node)
construct root = go (asNode root)
  where
    go        :: DOM :> es => Node -> Html (Constant t) a -> Eff es (Html (Constant t) Node)
    go parent = \case
      TextNode _ text       -> do node <- asChildOf parent $ createTextNode (coerce text)
                                  pure $ TextNode node text
      Element tag _ ats chs -> do node <- asChildOf parent $ createElement tag
                                  chs' <- traverse (go node)
                                                   (coerce @_ @(Seq.Seq (Html (Constant _) _)) chs)
                                  sequenceA_ [ setAttribute node attr (getConstant val)
                                             | (attr,val) <-
                                                 Map.toAscList (getConstant ats)
                                             ]
                                  pure $ Element tag node ats (coerce chs')

--------------------------------------------------------------------------------

asChildOf               :: (IsNode parent, IsNode child, DOM :> es)
                        => parent -> Eff es child -> Eff es child
asChildOf parent create = do new <- create
                             appendChild parent new
                             pure new

tailwindCss :: URL
tailwindCss = URL "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"

flowByte :: URL
flowByte = URL "https://cdn.jsdelivr.net/npm/flowbite@3.1.2/dist/flowbite.min.css"

main :: IO ()
main = runEff . evalDOM $ withRuntime myMain
  where
    myMain     :: forall t es ls. ( ls ~ '[ DOM , IOE ]
                                  , DOM :> es
                                  , Subset ls es
                                  , HasRuntime ls t :> es
                                  )
               => Ctx ls t -> Eff es ()
    myMain ctx = do
      void $ appendScript tailwindCss


      body <- jsBody

      --------------------------------------------------------------------------------
      minButton  <- asChildOf body      $ createElement (ElementName "button")
      minText    <- asChildOf minButton $ createTextNode "-"

      textValue  <- asChildOf body $ createTextNode "initial text"

      plusButton <- asChildOf body $ createElement (ElementName "button")
      plusText   <- asChildOf plusButton $ createTextNode "+"

      doubleValue  <- asChildOf body $ createTextNode "initial double value"

      --------------------------------------------------------------------------------

      minButton2  <- asChildOf body      $ createElement (ElementName "button")
      minText2    <- asChildOf minButton2 $ createTextNode "-"

      combinedValue  <- asChildOf body $ createTextNode "combined text"

      plusButton2 <- asChildOf body $ createElement (ElementName "button")
      plusText2   <- asChildOf plusButton2 $ createTextNode "+"

      --------------------------------------------------------------------------------

      construct body myHtml

      --------------------------------------------------------------------------------

      -- withSignal ctx 0 $ \counter -> do
      counter  <- createSignal ctx 0
      counter2 <- createSignal ctx 0

      let doubleCounter = Derive counter (*2)

      let handler     :: Event -> Eff (HasRuntime ls t : ls) ()
          handler evt = do
              consoleLog "- clicked"
              v <- modifySignal ctx counter pred
              setTextContent textValue ("counter 1 : " <> Text.show v)

          myMinEffect :: Eff (HasRuntime ls t : ls) ()
          myMinEffect = void $ addEventListener minButton (EventName "click") handler

      createEffect_ ctx myMinEffect

      createEffect_ ctx $ do
            void $ addEventListener' plusButton (EventName "click") $ \evt -> do
              consoleLog "+ clicked"
              setTextContent textValue "+ clicked"
              v <- modifySignal ctx counter succ
              setTextContent textValue ("counter 1 : " <> Text.show v)

      createEffect_ ctx $ do
        let f x = "double counter1 value: " <> Text.show x
        setTextContent doubleValue . f =<< current ctx doubleCounter


      --------------------------------------------------------------------------------
      createEffect_ ctx $ do
            void $ addEventListener' plusButton2 (EventName "click") $ \evt -> do
              consoleLog "+ button 2 clicked"
              modifySignal_ ctx counter2 succ
      createEffect_ ctx $ do
            void $ addEventListener' minButton2 (EventName "click") $ \evt -> do
              consoleLog "- button 2 clicked"
              modifySignal_ ctx counter2 pred

      let combined = (+) <$> varying counter <*> varying counter2
          combinedText = (\x -> "combined text" <> Text.show x)
                         <$> combined

      createEffect_ ctx $ do
        setTextContent combinedValue =<< current ctx combinedText

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------


addEventListener' :: forall ls target t.
                    ( IsEventTarget target
                    , DOM :> ls
                    -- , ls ~ '[IOE]
                    )
                 => target -> EventName
                 -> (Event -> Eff (HasRuntime ls t : ls) ())
                 -> Eff (HasRuntime ls t : ls) JsEventListener
addEventListener' = addEventListener

  -- undefined -- addEventListenerWith runEff

{-

-- | Runs an event handler
addEventListenerWith                              :: forall ls target t.
                                                 ( IsEventTarget target
                                                 , IOE :> ls
                                                 )
                                              => (Eff ls () -> IO ())
                                              -> target -> EventName
                                              -> (JSVal -> Eff (HasRuntime ls t : ls) ())
                                              -> Eff (HasRuntime ls t : ls) JsEventListener
addEventListenerWith embed target (EventName e) handler = do
  runtimeRef <- getStateMVar
  let run :: Eff (HasRuntime ls t : ls) () -> IO ()
      run = embed . evalStateMVar runtimeRef
  liftIO $ js_addEventListener (asEventTarget target)
                               (textToJSString e)
                               (run . handler)

-}

-- setTextContent        :: (IsNode textNode, IOE :> ls) => textNode -> Text -> Eff ls ()
-- setTextContent node t = liftIO $ js_set_text_content (asNode node) (textToJSString t)

-- consoleLog :: IOE :> ls => Text -> Eff ls ()
-- consoleLog = liftIO . js_log . textToJSString
