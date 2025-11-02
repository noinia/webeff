{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
module Main where


import Control.Monad
import WebEff.Reactive
import WebEff.FFI
import WebEff.Varying
import WebEff.Signal.Derived
import WebEff.FFI.Types
import Data.Coerce
import Data.IntMap (IntMap)
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

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------










  -- do modify $ \runTime ->
  --                            runTime {

  --                                    }


--------------------------------------------------------------------------------

-- data HList (ts :: [Type]) where
--   HNil  :: HList '[]
--   HCons :: t -> HList ts -> HList (t : ts)


-- type family as ++ bs where
--   '[]      ++ bs = bs
--   (a : as) ++ bs = a : (as ++ bs)

-- class HConcat list where
--   hConcat :: list as -> list bs -> list (as ++ bs)

-- instance HConcat HList where
--   hConcat as bs = case as of
--     HNil        -> bs
--     HCons a as' -> HCons a (hConcat as' bs)

-- class HSplit as where
--   hSplit :: (cs ~ as ++ bs) => HList cs -> (HList as, HList bs)

-- instance HSplit '[] where
--   hSplit bs = (HNil, bs)

-- instance HSplit ts => HSplit (t:ts) where
--   hSplit (HCons a ts) = let (as, bs) = hSplit ts
--                         in (HCons a as, bs)


-- data SigList t (as :: [Type]) where
--   SigNil  ::                               SigList t '[]
--   SigCons :: Signal t a -> SigList t as -> SigList t (a : as)

-- instance HConcat (SigList t) where
--   hConcat as bs = case as of
--     SigNil        -> bs
--     SigCons a as' -> SigCons a (hConcat as' bs)


-- data Varying t b where
--   Varying :: SigList t as -> (HList as -> b) -> Varying t b

--     -- Signal t a -> (a -> b) -> Varying t b

-- instance Functor (Varying t) where
--   fmap f (Varying signal g) = Varying signal (f . g)

-- instance Applicative (Varying t) where
--   pure x = Varying SigNil (const x)

--   -- func :: Signal t i
--   -- g    :: (i -> (a -> b))
--   --
--   -- signal :: Signal t j
--   -- h      :: j -> a
--   (Varying func g) <*> (Varying sigs h) = Varying (hConcat func sigs) go
--     where
--       go sigs = let (fs, as) = hSplit sigs
--                 in traverse (getSignal ..) fs


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

asChildOf               :: (IsNode parent, IsNode child, DOM :> es)
                        => parent -> Eff es child -> Eff es child
asChildOf parent create = do new <- create
                             appendChild parent new
                             pure new

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
