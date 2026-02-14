{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
module WebEff(main) where

import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Functor.Classes
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import WebEff.DOM
import WebEff.FFI.Types
import WebEff.Reactive
import WebEff.Runtime
import WebEff.Signal.Derived
import WebEff.Varying
-- import Data.Functor.Apply qualified as Apply
import Effectful.Dispatch.Dynamic (interpret, send)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------










  -- do modify $ \runTime ->
  --                            runTime {

  --                                    }


--------------------------------------------------------------------------------

type View t node evt = Html (Varying t) node evt

-- | Data type modelling a Html Tree. All actual values are wrapped in
-- an 'f' functor.  furthermore, we are storing a value of type 'node'
-- at every node. Event handlers are of type 'evt'
data Html f node evt = TextNode node (f Text)
                     | Element  ElementName node (f (Map.Map AttributeName (f Text            )))
                                                 (f (Map.Map EventName     (f evt             )))
                                                 (f (Seq.Seq               (f (Html f node evt))))
            -- todo; it's abit weird that attributes are text
  deriving stock (Functor, Foldable, Traversable)


deriving instance (Show1 f, Show node, Show evt) => Show (Html f node evt)
deriving instance (Eq1 f, Eq node, Eq evt)       => Eq   (Html f node evt)

-- | Map the f's to g's
bmap      :: forall f g node evt. Functor f
          => (forall a. f a -> g a) -> Html f node evt -> Html g node evt
bmap ftog = go
  where
    go = \case
      TextNode n text            -> TextNode n (ftog text)
      Element tag n ats evts chs -> Element tag n (ftog $ applyAts <$> ats)
                                                  (ftog $ applyAts <$> evts)
                                                  (ftog $ applyChs <$> chs)

    applyAts :: forall k a. Map.Map k (f a) -> Map.Map k (g a)
    applyAts = fmap ftog

    applyChs :: Seq.Seq (f (Html f node evt)) -> Seq.Seq (g (Html g node evt))
    applyChs = fmap (ftog . fmap go)

-- I t hink this needs h to be a monad, moreover we need to pick whether to
-- traverse top down or bottom up

-- | Map the f's to g's
traverseF      :: forall f g h node evt. (Functor f, Monad h)
               => (forall a. f a -> h (g a)) -> Html f node  evt -> h (Html g node evt)
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
textNode_ :: Applicative f => Text -> Html f () evt
textNode_ = TextNode () . pure


data Attr f a msg = Attribute {-# UNPACK #-}!AttributeName (f a)
                  | OnEvent   {-# UNPACK #-}!EventName     (f msg)


-- | Constructs an element with fixed children (but each child itself
-- is properly wrapped in an f)
el_             :: forall f evt. Applicative f
                => ElementName
                -- ^ The element we are constructing
                -> [f (Attr f Text evt)]
                -- ^ The Attributes. The outer f may be used to adapt
                -- each individual attribute.
                -> [f (Html f () evt)]
                -- ^ Children
                -> Html f () evt
el_ tag ats chs = Element tag () attrs evts (pure $ Seq.fromList chs)
  where
    attrs = fmap fold ats'
    ats' :: f [Map.Map AttributeName (f Text)]
    ats' = traverse (fmap toAttrMap) ats

    toAttrMap :: Attr f Text evt -> Map.Map AttributeName (f Text)
    toAttrMap = \case
      Attribute name val -> Map.singleton name val
      _                  -> Map.empty

    evts = fmap fold evts'

    evts' :: f [Map.Map EventName (f evt)]
    evts' = traverse (fmap toEventMap) ats

    toEventMap :: Attr f Text evt -> Map.Map EventName (f evt)
    toEventMap = \case
      OnEvent name val -> Map.singleton name val
      _                -> Map.empty



merakibtn = "px-6 py-2 font-medium tracking-wide text-white capitalize transition-colors duration-300 transform bg-blue-600 rounded-lg hover:bg-blue-500 focus:outline-none focus:ring focus:ring-blue-300 focus:ring-opacity-80"


--------------------------------------------------------------------------------
data MyClicked :: Effect where
  MyClicked :: Event -> MyClicked m ()

type instance DispatchOf MyClicked = Dynamic


runMyClicked :: DOM :> es => Eff (MyClicked : es) a -> Eff es a
runMyClicked = interpret $ \_ -> \case
  MyClicked _ -> consoleLog "clicked!"

--------------------------------------------------------------------------------

myHtml :: (DOM :> es, MyClicked :> es) => Html (Constant t) () (Act es)
myHtml = div_ []
              [ pure $ button_ [ pure (id_ $ pure "minButton")
                               , pure (classes_ $ pure [ merakibtn
                                                       ]
                                      )
                               ]
                               [ pure $ textNode_ "-" ]
              , pure $ textNode_ "woei"
              , pure $ button_ [ pure (classes_ $ pure [merakibtn])
                               , pure (onClick_ $ pure MyClicked)
                               ]
                               [pure $ textNode_ "+"]
              ]

-- | Renders clasess
classes_ :: (Functor f, Foldable list) => f (list Text)
         -> Attr f Text evt
classes_ = class_ . fmap (Text.unwords . toList)

class_   :: f Text -> Attr f Text evt
class_ = Attribute (AttributeName "class")

id_     :: f Text  -> Attr f Text evt
id_ = Attribute (AttributeName "id")


div_ :: Applicative f => [f (Attr f Text evt)] -> [f (Html f () evt)] -> Html f () evt
div_ = el_ (ElementName "div")

button_ :: Applicative f => [f (Attr f Text evt)] -> [f (Html f () evt)] -> Html f () evt
button_ = el_ (ElementName "button")


type EventHandler e es = Event -> e (Eff es) ()

-- | Data type representing the type of actions we can take;
data Act es where
  -- | Every action is actually just a function from Event to an Effect
  Handler :: ( e :> es, DispatchOf e ~ Dynamic) => EventHandler e es  -> Act es

onEvent_              :: (Functor f, e :> es, DispatchOf e ~ Dynamic)
                      => EventName -> f (EventHandler e es) -> Attr f Text (Act es)
onEvent_ eventName fh = OnEvent eventName (Handler <$> fh)

onClick_ :: (Functor f, e :> es, DispatchOf e ~ Dynamic)
         => f (EventHandler e es) -> Attr f Text (Act es)
onClick_ = onEvent_ (EventName "click")

-- onClick_   :: ( DispatchOf e ~ Dynamic
--               , e :> es
--               , Functor f
--               )
--            => f (Event -> e (Eff es) ()) -> Attr f Text (EventAct es)
-- onClick_ h = OnEvent (EventName "click") ((send .) <$> h)

-- $ pure $ \_evt -> consoleLog "clicked!"

----------------------------------------

data CreateInfo t where
  CreatedTextNode :: Node
                  -- ^ Ref to the node we created
                  -> RegisteredEffect t ()
                  -- ^ Effect that sets the text
                  -> Maybe (RegisteredEffect t ())
                  -- ^ Effect to create and add the node
                  -> CreateInfo t


-- | Constructs a text node of the given parent; the content will automatically update
constructVaryingTextNode                 :: ( IsNode parent, DOM :> es
                                            , HasRuntime ls t :> es, Subset ls es
                                            , DOM :> ls
                                            )
                                         => Ctx ls t
                                         -> parent -> Varying t Text
                                         -> Eff es (Html (Varying t) (CreateInfo t) (Act es))
constructVaryingTextNode ctx parent text =
  do node       <- asChildOf parent $ createTextNode ""
     (effIx, _) <- createEffect ctx $ current ctx text >>= setTextContent node
     pure $ TextNode (CreatedTextNode node effIx Nothing) text

-- constructVaryingTextNode'                 :: ( IsNode parent, DOM :> es
--                                             , HasRuntime ls t :> es, Subset ls es
--                                             , DOM :> ls
--                                             )
--                                           => Ctx ls t
--                                           -> parent
--                                           -> Text -- initial text
--                                           -> Varying t (Maybe Text)
--                                           -> Eff es (Html (Varying t) (CreateInfo t) (Act es))
-- constructVaryingTextNode' ctx parent i text = do
--   createEffect ctx $ current text >>= \case
--     Nothing -> pure Nothing
--     Just t  -> pure $ TextNode

-- constructNode :: Ctx ls t -> parent -> Varying (Html (Varying t) a (Act es))


-- -- | renders the given Html tree
-- constructVarying      :: forall ls t es parent a.
--                          ( IsNode parent, DOM :> es
--                          , HasRuntime ls t :> es, Subset ls es
--                          , DOM :> ls
--                          )
--                       => Ctx ls t
--                       -> parent
--                       -> Html (Varying t) a (Act es)
--                       -> Eff es (Html (Varying t) Node (Act es))
-- constructVarying ctx root = go (asNode root)
--   where
--     go        :: Node -> Html (Varying t) a (Act es)
--               -> Eff es (Html (Varying t) Node (Act es))
--     go parent = \case
--       TextNode _ text            ->
--           do node   <- asChildOf parent $ createTextNode ""
--              _effIx <- createEffect ctx $ current ctx text >>= setTextContent node
--              pure $ TextNode node text
--       Element tag _ vAts vEvts vChs ->
--           do node <- asChildOf parent $ createElement tag
--              _    <- createEffect ctx $ do
--                chs  <- current ctx vChs
--                chs' <- traverse (\c -> do _ <- createEffect ctx $ current ctx >>= go node

--                                             ) chs

--              -- sequenceA_ [ setAttribute node attr (getConstant val)
--              --            | (attr,val) <-
--              --                Map.toAscList (getConstant ats)
--              --            ]

--              -- maybe we should store the JsEventListener's
--              -- this returns.
--              sequenceA_ [ case getConstant handler of
--                             Handler h ->
--                               void $ addEventListener' node event (send . h)
--                         | (event,handler) <-
--                             Map.toAscList (getConstant evts)
--                         ]

--              pure $ Element tag node ats evts (coerce chs')

--     go' :: Node -> Varying t
-- (go node)
--                               (coerce @_ @(Seq.Seq (Html (Varying _) _ _)) chs)




-- | renders the given Html tree
construct      :: (IsNode parent, DOM :> es)
               => parent
               -> Html (Constant t) a (Act es)
               -> Eff es (Html (Constant t) Node (Act es))
construct root = go (asNode root)
  where
    go        :: DOM :> es
              => Node -> Html (Constant t) a (Act es)
              -> Eff es (Html (Constant t) Node (Act es))
    go parent = \case
      TextNode _ text            ->
          do node <- asChildOf parent $ createTextNode (coerce text)
             pure $ TextNode node text
      Element tag _ ats evts chs ->
          do node <- asChildOf parent $ createElement tag
             chs' <- traverse (go node)
                     (coerce @_ @(Seq.Seq (Html (Constant _) _ _)) chs)
             sequenceA_ [ setAttribute node attr (getConstant val)
                        | (attr,val) <- Map.toAscList (getConstant ats)
                        ]
             -- maybe we should store the JsEventListener's
             -- this returns.
             sequenceA_ [ case getConstant handler of
                            Handler h -> void $ addEventListener' node event (send . h)
                        | (event,handler) <- Map.toAscList (getConstant evts)
                        ]

             pure $ Element tag node ats evts (coerce chs')


--------------------------------------------------------------------------------

-- | Given a parent and some code to create a child node, run the
-- cration code and add it append it to the children of the parent.
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

      runMyClicked $ construct body myHtml

      --------------------------------------------------------------------------------

      -- withSignal ctx 0 $ \counter -> do
      counter  <- createSignal ctx 0
      counter2 <- createSignal ctx 0

      let doubleCounter = Derive counter (*2)



      -- let handler     :: Event -> Eff (HasRuntime ls t : ls) ()
      let minEffect :: Eff (HasRuntime ls t : ls) ()
          minEffect = do
            consoleLog "minEffect fired"
            v <- getSignal ctx counter
            setTextContent textValue ("counter 1 : " <> Text.show v)

          -- handler     :: Event -> Eff es ()
          -- handler evt =
          --   -- createEffect_ ctx $ do
          --     -- if we use evt we should be careful; since we want to use the latest event.
          --     -- not the original one

      minClickedEffect <- registerEffect minEffect
          -- myMinEffect :: Eff (HasRuntime ls t : ls) ()
          -- myMinEffect = void $ addEventListener minButton (EventName "click") handler
      void $ addEventListener' minButton (EventName "click") $ \_evt -> do
                consoleLog "- clicked"
                modifySignal ctx counter pred
                runEffect ctx minClickedEffect

      -- myMinEffect

      let plusEff :: Eff (HasRuntime ls t : ls) ()
          plusEff = do
              consoleLog "plusEff fired"
              setTextContent textValue "+ clicked"
              v <- getSignal ctx counter
              setTextContent textValue ("counter 1 : " <> Text.show v)

      plusClickedEffect <- registerEffect plusEff



      void $ addEventListener' plusButton (EventName "click") $ \_evt -> do
          consoleLog "+ clicked"
          modifySignal ctx counter succ
          runEffect ctx plusClickedEffect

      createEffect_ ctx $ do
        let f x = "double counter1 value: " <> Text.show x
        setTextContent doubleValue . f =<< current ctx doubleCounter


      --------------------------------------------------------------------------------

{-
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
-}

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------


addEventListener' :: forall es target t.
                    ( IsEventTarget target
                    , DOM :> es
                    -- , ls ~ '[IOE]
                    )
                 => target -> EventName
                 -> (Event -> Eff es ())
                 -> Eff es JsEventListener
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
