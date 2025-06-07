{-# LANGUAGE RecordWildCards #-}
module WebEff.App
  ( AppSpec(..)
  , Updated(..)
  , View

  , runApp
  ) where

import           Data.Foldable
import           Data.Monoid
import           Effectful
import           Effectful.Concurrent
import           Effectful.Concurrent.STM
import           WebEff.DOM
import           WebEff.DOM.FFI (jsBody, CanRunHandler, runCanRunHandler)
import qualified WebEff.DOM.FFI.Types as FFI
import           WebEff.Send
import           WebEff.Updated

--------------------------------------------------------------------------------

-- | Data type declaring what an WebEffApp is
data AppSpec es model msg =
  AppSpec { render         :: model -> View () msg
            -- ^ Function used to render the view
          , controller     :: model -> msg -> Eff es (Updated model)
            -- ^ Our controller that dispatches how to handle actions
          , initialMessage :: Maybe msg
            -- ^ Some initial message to fire upon starting the app
          , initialModel   :: model
            -- ^ The intitial model
          }

-- | A view is just a HtmlTree
type View a msg = Html (HandlerEs msg) a msg



--------------------------------------------------------------------------------

-- | Maximum number of unhandled events we can have in our queue at any time.
queueSize = 1024

--------------------------------------------------------------------------------

-- | Runs a WebEff app
runApp          :: forall appEs es model msg.
                   ( Concurrent :> es
                   , DOM        :> es
                   , Subset appEs es
                   , DOM        :> appEs
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
            msg      <- atomically $ readTBQueue queue
            liftEff (controller currentModel msg) >>= \case
              Unchanged        -> process currentModel currentView
                                  -- model is unchanged, so therefore the view is
                                  -- unchanged as well
              Changed newModel -> do newView  <- runRender newModel
                                     -- TODO: this is not really right yet but whatever
                                     process newModel newView

        liftEff :: Eff appEs a -> Eff es a
        liftEff = inject -- for whatever reason ghc doesn't  like it if we inline this.

        handlerSetup :: Eff (HandlerEs msg) () -> IO ()
        handlerSetup = runEff . evalDOM . runConcurrent . runSendWith queue

type HandlerEs msg = [Send msg,Concurrent,DOM,IOE]



renderView                                :: forall es root msg a model.
                                             ( DOM :> es, FFI.IsNode root
                                             )
                                          => (Eff (HandlerEs msg) () -> IO ())
                                          -> root
                                          -> (model -> View a msg)
                                          -> model
                                          -> Eff es (View NodeRef msg)
renderView handlerSetup root render model =
  runCanRunHandler handlerSetup $ renderWith root (render model)
