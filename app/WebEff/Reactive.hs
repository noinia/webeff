module WebEff.Reactive
  ( Runtime
  , withRuntime
  , createRuntime


  , Signal
  , withSignal
  , getSignal
  , setSignal
  , modifySignal


  , createSignal, deleteSignal


  , RegisteredEffect
  , createEffect, createEffect_
  , runEffect
  , registerEffect


  , untypedGetSignalDyn
  ) where


import Control.Monad (void)
import Effectful.Exception (bracket)
import Data.Kind (Type)
import Data.Proxy
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Data.Coerce
import Control.Lens
import Data.EnumSet qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Data.Dynamic (Typeable)
import Effectful
import Effectful.State.Static.Shared
import Data.Dynamic.Lens qualified as LensDynamic
import Data.Dynamic.Lens (_Dynamic)
import WebEff.Runtime

--------------------------------------------------------------------------------

-- | Run some computation with a signal.
withSignal        :: forall ls t es a r. (HasRuntime ls t :> es, Typeable a)
                  => Ctx ls t
                  -> a
                  -- ^ the initial value for the signal
                  -> (Signal t a -> Eff es r)
                  -- ^ The computation to run
                  -> Eff es r
withSignal ctx x0 = bracket (createSignal ctx x0)
                            (const $ pure ())
                            -- (deleteSignal ctx)

-- FIXME: we currently don't actually delete the signa. If we do use
-- deleteSignal this does not really work as expected yet, as the
-- finalizer runs too early.


-- | Create a new Signal
createSignal      :: forall ls t es a. (HasRuntime ls t :> es, Typeable a)
                  => Ctx ls t -> a -> Eff es (Signal t a)
createSignal _ x0 = state $ \(runtime :: Runtime ls t) ->
                            let i     = runtime^.nextSignalId
                                sData = SignalData (Dynamic.toDyn x0) Set.empty
                            in ( Signal i :: Signal t a
                               , runtime&nextSignalId %~ succ
                                        &rawSignals   %~ IntMap.insert i sData
                               )

-- | Delete a signal. Note that this does not trigger any effects
deleteSignal          :: forall ls t es a. HasRuntime ls t :> es
                      => Ctx ls t -> Signal t a -> Eff es ()
deleteSignal _ signal = modify $ \(runtime :: Runtime ls t) ->
                                   runtime&rawSignals.at (coerce signal) .~ Nothing




-- | Access the signal value
getSignal            :: forall ls t es a. ( HasRuntime ls t :> es
                                          , Typeable a
                                          ) => Ctx ls t -> Signal t a -> Eff es a
getSignal ctx signal = state $ \(runtime :: Runtime ls t) ->
    runtime&signalAt' signal %%~ getAndSubscribe (runtime^.currentEffect)

-- | Get the current value of the signal, furthermore register the currently running
-- effect (if such an effect exists) as a subscriber of the signal.
--
-- returns the value, as well as the updated signal data (which
-- contains the updated) subscribers.
getAndSubscribe                    :: Maybe (RegisteredEffect t)
                                   -> SignalData t a
                                   -> (a, SignalData t a)
getAndSubscribe current signalData = ( signalData^.theValue
                                     , case current of
                                         Nothing  -> signalData
                                         Just eff -> signalData&subscribers %~ Set.insert eff
                                     )

-- | Access the signal value, the signal is represented as a raw Int
untypedGetSignalDyn          :: forall ls t es. ( HasRuntime ls t :> es)
                             => Ctx ls t -> Int -> Eff es Dynamic.Dynamic
untypedGetSignalDyn _ signal = state $ \(runtime :: Runtime ls t) ->
    runtime&untypedSignalAtDyn signal %%~ getAndSubscribe (runtime^.currentEffect)
  where
    untypedSignalAtDyn i = singular (rawSignals.at i._Just)

-- | Set the signal to a given value. (Possibly registering the
-- current event as a subscriber). This
setSignal            :: forall ls t es a. ( HasRuntime ls t :> es
                                          , Subset ls es
                                          , Typeable a
                                          )
                     => Ctx ls t -> Signal t a -> a -> Eff es ()
setSignal _ signal x = do
      -- set the value, and get the current subscribers
      subs <- state $ \(runtime :: Runtime ls t) ->
                        runtime&signalAt' signal %%~ \sigData ->
                                     (sigData^.subscribers, sigData&theValue .~ x)
      traverse_ rerun (Set.elems subs)
  where
    rerun       :: RegisteredEffect t -> Eff es ()
    rerun effIx = do (eff :: Eff (HasRuntime ls t:ls) ()) <- gets (^.effectAt effIx)
                     runEffect effIx eff

-- | Access and update the signal value. Returns the new value. This
-- triggers re-running the effects that subscribe to this signal
modifySignal              :: forall ls t es a. ( HasRuntime ls t :> es
                                               , Subset ls es
                                               , Typeable a
                                               )
                          => Ctx ls t
                          -> Signal t a
                          -> (a -> a)
                           -- ^ update function
                          -> Eff es a
modifySignal ctx signal f = do
    (signalData, current) <- state $ \(runtime :: Runtime ls t) ->
                                       runtime&signalAt' signal %%~ \sigData ->
                                           let sigData' = sigData&theValue %~ f
                                           in ((sigData',runtime^.currentEffect),sigData')
    traverse_ rerun (signalData^.subscribers.to Set.elems)
    subscribeCurrentEffectTo ctx signal current
    pure $ signalData^.theValue
  where
    rerun       :: RegisteredEffect t -> Eff es ()
    rerun effIx = do (eff :: Eff (HasRuntime ls t : ls) ()) <- gets (^.effectAt effIx)
                     runEffect effIx eff

-- | if we have a current effect, add it as a subscriber
subscribeCurrentEffectTo          :: forall ls t es a. (HasRuntime ls t :> es)
                                  => Ctx ls t
                                  -> Signal t a -> Maybe (RegisteredEffect t) -> Eff es ()
subscribeCurrentEffectTo _ signal = \case
  Nothing  -> pure ()
  Just eff -> modify $ \(runtime :: Runtime ls t) ->
                         runtime&singular (signalAtDyn signal).subscribers %~ Set.insert eff


-- reRun :: f (RegisteredEffect t) -> Eff es ()
-- reRun = traverse

-- instance Foldable Set.EnumSet where
--   foldMap f


--------------------------------------------------------------------------------

-- | Run some effectful computation using a local state.
withLocalState       :: (State s :> es)
                     => (s -> (old, s))
                     -- ^ function to re-initialize the state
                     -> (old -> s -> s)
                     -- ^ function that recombines the previous state and the newer
                     -- state (i.e. after running the action) into a final state
                     -> Eff es a
                     -- ^ The effect to run with the local state
                     -> Eff es a
withLocalState initialize recombine act = do old  <- state initialize
                                             x    <- act
                                             modify $ recombine old
                                             pure x


--------------------------------------------------------------------------------

-- | Create a new registered effect and run it.
createEffect         :: ( HasRuntime ls t :> es, Subset ls es)
                     => Ctx ls t -> Eff (HasRuntime ls t : ls) () -> Eff es (RegisteredEffect t)
createEffect ctx eff = do
    effIx <- registerEffect eff
    runEffect effIx eff
    pure effIx

-- | Create a new registered effect and run it.
createEffect_         :: ( HasRuntime ls t :> es, Subset ls es )
                      => Ctx ls t -> Eff (HasRuntime ls t : ls) () -> Eff es ()
createEffect_ ctx eff = void $ createEffect ctx eff

-- | Runs the local effect
runEffect           :: forall ls t es.
                       ( HasRuntime ls t :> es
                       , Subset ls es
                       )
                    => RegisteredEffect t
                    -> Eff (HasRuntime ls t : ls) ()
                    -> Eff es ()
runEffect effIx eff = withLocalState (currentEffect %%~ \oldIx -> (oldIx, Just effIx))
                                     recombine  -- we restore only the currentEffect
                                     (inject eff)
  where
    recombine                :: Maybe (RegisteredEffect t) -> Runtime ls t -> Runtime ls t
    recombine oldIx newState = newState&currentEffect .~ oldIx


-- | Create/register a new registered effect.
registerEffect   :: forall ls t es.
                    (HasRuntime ls t :> es)
                 => Eff (HasRuntime ls t : ls) ()
                 -> Eff es (RegisteredEffect t)
registerEffect f = state $ \(runtime :: Runtime ls t) ->
                             let i = runtime^.nextEffectId
                             in ( RegisteredEffect i :: RegisteredEffect t
                                , runtime&nextEffectId %~ succ
                                         &rawEffects   %~ IntMap.insert i f
                                )


--------------------------------------------------------------------------------

-- -- | Create a new registered effect and run it.
-- createEffect'     :: forall ls t.
--                      ( HasRuntime ls t :> s
--                      )
--                   => Eff ls ()
--                   -> Eff ls (RegisteredEffect t)
-- createEffect' eff = do
--     effIx <- registerEffect eff
--     runEffect' effIx eff
--     pure effIx

-- -- | Runs the local effect
-- runEffect'           :: forall ls t es.
--                        -- ( HasRuntime ls t :> es
--                        -- , Subset ls es
--                        -- )
--                     => RegisteredEffect t
--                     -> Eff ls ()
--                     -> Eff es ()
-- runEffect' effIx eff = undefined

  -- do State mvar <- evalStaticRep
  --                         evalStateMVar
