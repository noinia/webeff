{-# LANGUAGE RecordWildCards #-}
module WebEff.App
  ( AppSpec(..)
  , View

  , runApp
  ) where

import           Data.Foldable
import           Data.Monoid
import           Effectful
import           Effectful.Concurrent
import           Effectful.Concurrent.STM
import           WebEff.DOM
import           WebEff.DOM.FFI (jsBody)
import qualified WebEff.DOM.FFI.Types as FFI
import           WebEff.Send
--------------------------------------------------------------------------------

-- | Data type declaring what an WebEffApp is
data AppSpec es model msg =
  AppSpec { render         :: model -> View () msg
          , controller     :: model -> msg -> Eff es model
          , initialMessage :: Maybe msg
          , initialModel   :: model
          }

type View a msg = Html a msg


-- type View' a msg = Html a msg

--------------------------------------------------------------------------------

queueSize = 1024

--------------------------------------------------------------------------------


-- runRender :: model -> View' NodeRef msg -> View' NodeRef msg -> Eff es ()
-- runRender = undefined


-- | Runs a WebEff app
runApp          :: forall appEs es model msg.
                   ( Concurrent :> es
                   , DOM        :> es
                   , Subset appEs es
                   )
                => AppSpec appEs model msg -> Eff es ()
runApp AppSpec{ .. } = do queue <- atomically $ do q <- newTBQueue queueSize
                                                   for_ initialMessage $ writeTBQueue q
                                                   pure q
                          body  <- jsBody
                          startApp queue body
  where
    startApp            :: TBQueue msg -> FFI.Body -> Eff es ()
    startApp queue body = do
          -- creates the initial view
          theInitialView <- runRender initialModel
          -- start processing events
          process initialModel theInitialView
      where
        runRender :: model -> Eff es (View NodeRef msg)
        runRender = renderView handlerSetup body render

        process                          :: model -> View NodeRef msg -> Eff es ()
        process currentModel currentView = do
            msg <- atomically $ readTBQueue queue
            newModel <- liftEff $ controller currentModel msg
            newView  <- runRender newModel -- TODO: this is not really right yet but whatever
            process newModel newView

        liftEff :: Eff appEs a -> Eff es a
        liftEff = inject -- for whatever reason ghc doesn't  like it if we inline this.

        handlerSetup :: Eff [Send msg,Concurrent,DOM,IOE] () -> IO ()
        handlerSetup = runEff . evalDOM . runConcurrent . runSendWith queue



renderView                                :: (DOM :> es, FFI.IsNode root)
                                          => (Eff syncEs () -> IO ())
                                          -> root
                                          -> (model -> View a msg)
                                          -> model
                                          -> Eff es (View NodeRef msg)
renderView handlerSetup root render model = renderWith root (render model)
