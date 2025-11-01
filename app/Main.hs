{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
module Main where


import Control.Monad
import WebEff.Reactive
import WebEff.FFI
import WebEff.FFI.Types
import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Effectful
import WebEff.SharedState
import Control.Lens
import WebEff.Runtime
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful.Dispatch.Static

import GHC.Wasm.Prim (JSVal)

--------------------------------------------------------------------------------

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------










  -- do modify $ \runTime ->
  --                            runTime {

  --                                    }


--------------------------------------------------------------------------------





main :: IO ()
main = do
  body <- js_body
  minButton  <- js_createElement (textToJSString "button")
  minText    <- js_createTextNode (textToJSString "-")

  textValue  <- js_createTextNode (textToJSString "initial text")

  plusButton <- js_createElement (textToJSString "button")
  plusText  <- js_createTextNode (textToJSString "+")

  js_appendChild (coerce body) minButton
  js_appendChild minButton minText

  js_appendChild (coerce body) textValue

  js_appendChild (coerce body) plusButton
  js_appendChild plusButton plusText

  let
      myMain     :: forall t es ls. (ls ~ '[ IOE
                                           ]
                                    , IOE :> es
                                    , HasRuntime ls t :> es
                                    )
                 => Ctx ls t -> Eff es ()
      myMain ctx = do
        -- withSignal ctx 0 $ \counter ->
        -- withSignal does not really work yet; as it should somwhow wait indefinitely

        counter <- createSignal ctx 0

        let handler     :: JSVal -> Eff (HasRuntime ls t : ls) ()
            handler evt = do
                consoleLog "- clicked"
                v <- modifySignal ctx counter pred
                setTextContent textValue (Text.show v)

            myMinEffect :: Eff (HasRuntime ls t : ls) ()
            myMinEffect = void $ addEventListener minButton (EventName "click") handler

        void $ createEffect ctx myMinEffect

        void $ createEffect ctx $ do
              void $ addEventListener plusButton (EventName "click") $ \evt -> do
                consoleLog "+ clicked"
                setTextContent textValue "+ clicked"
                v <- modifySignal ctx counter succ
                setTextContent textValue (Text.show v)

        liftIO $ print "boe"


  runEff $ withRuntime myMain

  putStrLn "woei"


--------------------------------------------------------------------------------


addEventListener :: forall ls target t.
                    ( IsEventTarget target
                    , ls ~ '[IOE]
                    )
                 => target -> EventName
                 -> (JSVal -> Eff (HasRuntime ls t : ls) ())
                 -> Eff (HasRuntime ls t : ls) JsEventListener
addEventListener = addEventListenerWith runEff

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


setTextContent        :: (IsNode textNode, IOE :> ls) => textNode -> Text -> Eff ls ()
setTextContent node t = liftIO $ js_set_text_content (asNode node) (textToJSString t)

consoleLog :: IOE :> ls => Text -> Eff ls ()
consoleLog = liftIO . js_log . textToJSString
