{-# LANGUAGE OverloadedStrings  #-}
module Main where

import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import           Data.Text (Text)
import           Effectful
import           Effectful.Concurrent
import           Prelude hiding (div)
import           WebEff.App
import           WebEff.Attribute
import           WebEff.DOM
import qualified WebEff.DOM.FFI.Types as FFI
import           WebEff.Html

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

--------------------------------------------------------------------------------
-- * FFI







--------------------------------------------------------------------------------
data MyModel = MyModel { myMessage :: Text.Text
                       , position  :: Maybe (Int,Int)
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
  , initialModel   = MyModel "Initial" Nothing
  }

myView m = div []
               [ h1  [ "class" =: classes ["header", "someclass"]
                     , onClick -:  SayHello
                     , "id"    =: ("theHeader" :: Text)
                     ]
                     [ textNode "header!"
                     ]
               , div [] [p [ onClick        -: SetMsg "woei"
                           , "x-foo"        =: ("bar" :: Text)
                           , "Style"        =: ("border: 1px solid black; width: 200px; height: 100px;" :: Text)
                           , onPointerOver -: \pointerEvent -> UpdatePosition (client pointerEvent)
                           ]
                           [ textNode $ myMessage m
                           ]
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
  SetMsg t           -> pure $ Changed (m { myMessage = t})
  UpdatePosition pos -> Unchanged <$ consoleLog (Text.show pos)
