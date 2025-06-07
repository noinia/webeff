{-# LANGUAGE OverloadedStrings  #-}
module Main where

import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import           Effectful
import           Effectful.Concurrent
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
main = do
  putStrLn "Hello, Haskell!"
  runEff . runConcurrent . evalDOM $ runApp @'[DOM] myApp

--------------------------------------------------------------------------------
-- * FFI







--------------------------------------------------------------------------------
data MyModel = MyModel { myMessage :: Text.Text}

data MyMsg = SayHello
           | Skip


myApp :: DOM :> es => AppSpec es MyModel MyMsg
myApp = AppSpec
  { render         = myView
  , controller     = myUpdate
  , initialMessage = Just SayHello
  , initialModel   = MyModel "woei"
  }

myView m = div_ [ onClick_ SayHello ]
                [ p_ [] [text_ $ myMessage m]
                , text_ "test"
                ]

myUpdate   :: DOM :> es => MyModel -> MyMsg -> Eff es (Updated MyModel)
myUpdate m = \case
  SayHello -> Unchanged <$ consoleLog (myMessage m)
  Skip     -> pure Unchanged
