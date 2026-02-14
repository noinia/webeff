{-# LANGUAGE OverloadedStrings #-}
module WebEff.DOM
  ( DOM, evalDOM

  , jsDocument
  , jsBody
  , jsWindow
  , jsHead
  , getParent
  , consoleLog

  , createTextNode, setTextContent

  , createElement
  , removeSelf
  , appendChild, removeChild

  , setAttribute, removeAttribute
  , HasSetAttributeValue(..)

  -- , CanRunHandler
  -- , runCanRunHandler
  -- , EventHandlerRunner

  , addEventListener, removeEventListener


  , getProperty
  , setProperty

  , appendStyleSheet
  , appendScript
  ) where

import           Data.Coerce
import           Data.Text (Text)
import           Effectful
import           Effectful.Dispatch.Static
import           WebEff.FFI.Prim (JSVal)
import           WebEff.FFI.Classes
import           WebEff.FFI
import           WebEff.FFI.Types

--------------------------------------------------------------------------------

data DOM  :: Effect
type instance DispatchOf DOM  = Static WithSideEffects
newtype instance StaticRep DOM = MkDOM ()


--
evalDOM :: IOE :> es => Eff (DOM : es) a -> Eff es a
evalDOM = evalStaticRep (MkDOM ())

--------------------------------------------------------------------------------
-- * Global elements

jsDocument :: DOM :> es => Eff es Document
jsDocument = unsafeEff_ js_document

jsHead :: DOM :> es => Eff es Head
jsHead = unsafeEff_ js_head

jsBody :: DOM :> es => Eff es Body
jsBody = unsafeEff_ js_body

jsWindow :: DOM :> es => Eff es Window
jsWindow = unsafeEff_ js_window


--------------------------------------------------------------------------------
-- * Querying things

getParent :: (DOM :> es, IsNode element) => element -> Eff es Node
getParent = unsafeEff_ . coerce . js_getParent . asNode
  -- FIXME; the coerce here is hacky


--------------------------------------------------------------------------------
-- * Text

createTextNode :: DOM :> es => Text -> Eff es Node
createTextNode = unsafeEff_ . js_createTextNode . textToJSString

setTextContent :: (DOM :> es, IsNode node) => node -> Text -> Eff es ()
setTextContent node content = unsafeEff_
                            $ js_set_text_content (asNode node) (textToJSString content)

--------------------------------------------------------------------------------
-- * Elements

createElement                        :: DOM :> es => ElementName -> Eff es Node
createElement (ElementName elemName) = unsafeEff_ $ js_createElement (textToJSString elemName)

-- | Delete the current element
removeSelf      :: (DOM :> es, IsNode node) => node  -> Eff es ()
removeSelf node = unsafeEff_  $ js_removeSelf (asNode node)







appendChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
appendChild parent child = unsafeEff_  $ js_appendChild (asNode parent) (asNode child)


removeChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
removeChild parent child = unsafeEff_  $ js_removeChild (asNode parent) (asNode child)






--------------------------------------------------------------------------------
-- * Attributes

-- | pre: element is of type 'el'
setAttribute               :: forall es element value.
                              (IsNode element, DOM :> es, HasSetAttributeValue value)
                           => element -> AttributeName -> value
                           -> Eff es ()
setAttribute el (AttributeName attr) value = unsafeEff_ $
    js_setAttribute (asNode el) (textToJSString attr) value

-- | Removes an attribute
removeAttribute                         :: (IsNode element, DOM :> es)
                                        => element -> AttributeName -> Eff es ()
removeAttribute el (AttributeName attr) = unsafeEff_ $
    js_removeAttribute (asNode el) (textToJSString attr)

--------------------------------------------------------------------------------
-- Console

consoleLog :: DOM :> es => Text -> Eff es ()
consoleLog = unsafeEff_  . js_log . textToJSString





--------------------------------------------------------------------------------
-- * Events

-- | Runs an event handler
addEventListener                              :: forall target handlerEs es.
                                                 ( IsEventTarget target
                                                 , DOM :> es
                                                 , Subset handlerEs es
                                                 )
                                              => target -> EventName
                                              -> (Event -> Eff handlerEs ())
                                              -> Eff es JsEventListener
addEventListener target (EventName e) handler = inject act
  where
    act :: Eff handlerEs JsEventListener
    act = do
      -- this is essentially: withEffToIO SeqForkUnlift $ \unlift -> ...
      -- however, I didn't want the IOE in the allowed effects of handlerEs
      unsafeEff $ \es -> concUnliftIO es Ephemeral Unlimited $ \unlift ->
        js_addEventListener (asEventTarget target)
                            (textToJSString e)
                            (unlift . handler . Event)
      -- the call to unlift turns a 'Eff handlerEs a' into an 'IO a'


-- | Removes the given event listener
removeEventListener                 :: (IsEventTarget eventTarget, DOM :> es)
                                    => eventTarget
                                    -> EventName
                                    -> JsEventListener
                                    -> Eff es ()
removeEventListener target
                    (EventName eventType)
                    listener' = unsafeEff_ $
  js_remove_event_listener (asEventTarget target) (textToJSString eventType) listener'


--------------------------------------------------------------------------------

-- | Get a property of a particular item
getProperty :: ( DOM :> es
               , HasGetPropertyValue value
               , Coercible object JSVal
               ) => object -> PropertyName -> Eff es value
getProperty obj (PropertyName prop) = unsafeEff_ $
                                      js_getProperty (coerce obj) (textToJSString prop)

-- | Set a property
setProperty :: ( DOM :> es
               -- , HasGetPropertyValue value
               , Coercible object JSVal
               ) => object -> PropertyName -> Text -> Eff es ()
setProperty obj (PropertyName prop) val = unsafeEff_ $
    js_setProperty_String (coerce obj) (textToJSString prop) (textToJSString val)



--------------------------------------------------------------------------------


-- | Appends a stylesheet to thead of the page
appendStyleSheet     :: (DOM :> es) => URL -> Eff es Node
appendStyleSheet url = do hd   <- jsHead
                          link <- createElement (ElementName "link")
                          setProperty link (PropertyName "type") "text/css"
                          setProperty link (PropertyName "rel")  "stylesheet"
                          setProperty link (PropertyName "href") (coerce url)
                          appendChild hd link
                          pure link


-- | Appends a script to thead of the page
appendScript     :: (DOM :> es) => URL -> Eff es Node
appendScript url = do hd   <- jsHead
                      link <- createElement (ElementName "script")
                      setProperty link (PropertyName "src") (coerce url)
                      appendChild hd link
                      pure link


--------------------------------------------------------------------------------
{- -- the old event handler setup


-- | CanRunHandler is essentially a reader effect that stores the handler
data CanRunHandler (handlerEs :: [Effect]) :: Effect

type instance DispatchOf (CanRunHandler handlerEs) = Static NoSideEffects
newtype instance StaticRep (CanRunHandler handlerEs) =
  HandlerSetup (EventHandlerRunner handlerEs)

getHandlerSetup :: CanRunHandler handlerEs :> es => Eff es (EventHandlerRunner handlerEs)
getHandlerSetup = do HandlerSetup handlerSetup <- getStaticRep
                     pure handlerSetup

runCanRunHandler              :: EventHandlerRunner handlerEs
                              -> Eff (CanRunHandler handlerEs : es) a -> Eff es a
runCanRunHandler handlerSetup = evalStaticRep (HandlerSetup handlerSetup)

--------------------------------------------------------------------------------

-- | Type that explains how to actually run an EventHandler in IO
type EventHandlerRunner handlerEs = Eff handlerEs () -> IO ()


-- | Add an Event Listener.
addEventListener                           :: forall handlerEs es eventTarget msg.
                                              ( IsEventTarget eventTarget
                                              , DOM                     :> es
                                              , CanRunHandler handlerEs :> es
                                              )
                                           => eventTarget
                                           -> EventName
                                           -> (Event -> Eff handlerEs ())
                                           -> Eff es JsEventListener
addEventListener target (EventName eventType) listener = do
    -- get the eventHandlerRunner; i.e. the thing that we use to run the Eff hanlder () in the
    -- IO monad.
    (runListener :: Eff handlerEs () -> IO ()) <- getHandlerSetup
    let jsListener :: JSVal -> IO ()
        jsListener = runListener . listener . Event
    unsafeEff_  $
      js_addEventListener (asEventTarget target)
                          (textToJSString eventType)
                          jsListener


-}
