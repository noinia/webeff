{-# LANGUAGE OverloadedStrings  #-}
module Main where

import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Effectful
import           Effectful.Concurrent
import           Prelude hiding (div)
import           WebEff.App
import           WebEff.Attribute
import           WebEff.DOM
import qualified WebEff.DOM.FFI.Types as FFI
import           WebEff.Html as H hiding (main)
import           WebEff.Html.Attribute as A

-- import           Test.Hspec
import           WebEff.DOM.Tree

import           Data.Coerce
import           Effectful.Concurrent
import           Effectful.Concurrent.STM
import           WebEff.DOM.FFI (jsBody, CanRunHandler, runCanRunHandler, appendChild)
import           WebEff.Send

--------------------------------------------------------------------------------

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------


-- data Model model = Model { appModel :: model
--                          , queue    :: STM.TBQueue (Msg msg)
--                          }

-- data Msg msg = Render
--              | AppMsg msg

-- data WebEffApp es model msg =
--   WebEffApp




main :: IO ()
main = runEff . runConcurrent . evalDOM $ runApp @'[DOM] myApp
-- main = do
--   runEff . runConcurrent . evalDOM $ runApp @'[DOM] counterApp
{-
  runEff . runConcurrent . evalDOM $ do
    queue <- atomically $ newTBQueue 10
    body  <- jsBody

    -- creates the initial view
    theInitialView <- renderView (handlerSetup queue) body counterView 0

        -- -- debugging
    -- case diffHtml theInitialView (counterView 1) of
    --   Unchanged -> consoleLog "Unchanged"
    --   Changed (applyPatch,res) -> do consoleLog "Changed"
    --                                  consoleLog $ Text.show res
    --                                  runCanRunHandler (handlerSetup queue) applyPatch
    -- consoleLog "DONE"


    -- old@(TextData _ nodeRef) <- allocateTextData $ TextData "testText" ()
    -- appendChild body nodeRef

    -- case patch old (TextData "dummy" ()) of
    --   Unchanged -> consoleLog "Unchanged"
    --   Changed (applyPatch,res) -> do consoleLog "Text Changed"
    --                                  consoleLog $ Text.show res
    --                                  applyPatch

    -- consoleLog "DONE"

    -- oldNode@(ElemData _ node _ _) <- allocateElemData $ myDiv
    --                                  [(FFI.AttributeName "class", AttrValue ("foo" :: Text))]
    -- appendChild body node


    -- case patch @(ElemData (HandlerEs CounterMsg) CounterMsg)
    --              oldNode (myDiv [(FFI.AttributeName "class", AttrValue ("bar" :: Text))]) of
    --     Unchanged -> consoleLog "Unchanged"
    --     Changed (applyPatch,res) -> do consoleLog "Node Changed"
    --                                    consoleLog $ Text.show res
    --                                    runCanRunHandler (handlerSetup queue) $ applyPatch

    -- consoleLog "DONE"







  where
    renderView handlerSetup root render model =
      runCanRunHandler handlerSetup $ renderWith root (render model)

    handlerSetup       :: TBQueue CounterMsg -> Eff (HandlerEs CounterMsg) () -> IO ()
    handlerSetup queue = runEff . evalDOM . runConcurrent . runSendWith queue

    myDiv ats = ElemData (FFI.ElementName "div") () (Map.fromList ats) mempty


type HandlerEs msg = [Send msg,Concurrent,DOM,IOE]


    -- runEff . runConcurrent . evalDOM $ runApp @'[DOM] counterApp
-}

--------------------------------------------------------------------------------







--------------------------------------------------------------------------------
data MyModel = MyModel { myMessage :: Text.Text
                       , position  :: Maybe (Int,Int)
                       , numClicks :: Int
                       }


data MyMsg = SayHello
           | Skip
           | SetMsg Text
           | UpdatePosition (Int,Int)


myApp :: DOM :> es => AppSpec es MyModel MyMsg
myApp = AppSpec
  { render         = myView
  , controller     = myUpdate
  , initialMessage = Just SayHello
  , initialModel   = MyModel "Initial" Nothing 0
  }

myView m = div []
               [ h1  [ classes ["header", "someclass"]
                     , onClick SayHello
                     , A.id    "theHeader"
                     ]
                     [ textNode "header!"
                     ]
               , div [] [p [ onClick      $  SetMsg "woei"
                           , data_ "foo"     "bar"
                           , styleInline "border: 1px solid black; width: 200px; height: 100px;"
                           , onPointerMove $ \pointerEvent -> UpdatePosition (client pointerEvent)
                           ]
                           [ textNode $ "message: " <> myMessage m
                           , textNode $ "position: " <> Text.show (position m)
                           ]
                        , div [ classes ["foo","bar"] ]
                              (foldMap (\i -> [textNode $ Text.show i]) [1..(numClicks m)])
                        ]
               ]


-- myView m = div_ [ onClick_ SayHello ]
--                 [ p_ [] [text_ $ myMessage m]
--                 , text_ "test"
--                 ]

myUpdate   :: DOM :> es => MyModel -> MyMsg -> Eff es (Updated MyModel)
myUpdate m = \case
  SayHello           -> Unchanged <$ consoleLog (myMessage m)
  Skip               -> pure Unchanged
  SetMsg t           -> pure $ Changed (m { myMessage = t
                                          , numClicks = 1 + numClicks m
                                          }
                                       )
  UpdatePosition pos -> Changed (m { position = Just pos }) <$ consoleLog (Text.show pos)



--------------------------------------------------------------------------------


counterApp :: DOM :> es => AppSpec es Int CounterMsg
counterApp = AppSpec
  { render         = counterView
  , controller     = counterController
  , initialMessage = Nothing
  , initialModel   = 0
  }

data CounterMsg = Increment | Decrement
  deriving (Show)

counterController   :: DOM :> es => Int -> CounterMsg -> Eff es (Updated Int)
counterController m = \case
  Increment -> let m' = m + 1
               in Changed m' <$ consoleLog (Text.show m')
  Decrement -> let m' = m - 1
               in Changed m' <$ consoleLog (Text.show m')

counterView   :: Int -> View () CounterMsg
counterView m = div []
                    [ button [onClick Increment] [textNode "+"]
                    , button [onClick Decrement] [textNode "-"]
                    , textNode (Text.show m)
                    ]
