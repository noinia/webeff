{- HLINT ignore "Use tuple-section" -}
module WebEff.Reactive
  ( Runtime
  , withRuntime
  , createRuntime

  , HasCurrent(..)

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

  , Constant(..)
  ) where


import Control.Monad (void, when)
import Effectful.Exception (bracket)
import Data.Foldable
import Data.Coerce
import Control.Lens
import Data.EnumSet qualified as Set
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Data.Dynamic (Typeable)
import Effectful
import Effectful.State.Static.Shared
import WebEff.Runtime
import Data.Maybe (fromMaybe)

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
getSignal             :: forall ls t es a. ( HasRuntime ls t :> es
                                           , Typeable a
                                           ) => Ctx ls t -> Signal t a -> Eff es a
getSignal _ctx signal = state $ \(runtime :: Runtime ls t) ->
    runtime&signalAt' signal %%~ getAndSubscribe (runtime^.currentEffect)

-- | Get the current value of the signal, furthermore register the currently running
-- effect (if such an effect exists) as a subscriber of the signal.
--
-- returns the value, as well as the updated signal data (which
-- contains the updated) subscribers.
getAndSubscribe                    :: Maybe (RegisteredEffect t r)
                                   -> SignalData t a
                                   -> (a, SignalData t a)
getAndSubscribe current' signalData = ( signalData^.theValue
                                      , case current'' of -- forget the result type
                                          Nothing  -> signalData
                                          Just eff -> signalData&subscribers %~ Set.insert eff
                                      )
  where
    current'' = coerce current' :: Maybe (RegisteredEffect t Dynamic.Dynamic)

-- | Access the signal value, the signal is represented as a raw Int
untypedGetSignalDyn          :: forall ls t es. ( HasRuntime ls t :> es)
                             => Ctx ls t -> Int -> Eff es Dynamic.Dynamic
untypedGetSignalDyn _ signal = state $ \(runtime :: Runtime ls t) ->
    runtime&untypedSignalAtDyn signal %%~ getAndSubscribe (runtime^.currentEffect)
  where
    untypedSignalAtDyn i = singular (rawSignals.ix i)

-- | Set the signal to a given value. (Possibly registering the
-- current event as a subscriber). This
setSignal              :: forall ls t es a. ( HasRuntime ls t :> es
                                            , Subset ls es
                                            , Typeable a
                                            )
                       => Ctx ls t -> Signal t a -> a -> Eff es ()
setSignal ctx signal x = do
      -- set the value, and get the current subscribers
      subs <- state $ \(runtime :: Runtime ls t) ->
                        runtime&signalAt' signal %%~ \sigData ->
                                     (sigData^.subscribers, sigData&theValue .~ x)
      traverse_ (runEffect ctx) (Set.elems subs)


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
                          -> Eff es ()
modifySignal ctx signal f = do
    (signalData, current') <- state $ \(runtime :: Runtime ls t) ->
                                       runtime&signalAt' signal %%~ \sigData ->
                                           let sigData' = sigData&theValue %~ f
                                           in ((sigData',runtime^.currentEffect),sigData')
    traverse_ (runEffect ctx) (signalData^.subscribers.to Set.elems)
    subscribeCurrentEffectTo ctx signal current'
    -- pure $ signalData^.theValue

-- | if we have a current effect, add it as a subscriber
subscribeCurrentEffectTo                   :: forall ls t es a r. (HasRuntime ls t :> es)
                                           => Ctx ls t
                                           -> Signal t a -> Maybe (RegisteredEffect t r)
                                           -> Eff es ()
subscribeCurrentEffectTo _ signal current' = case current'' of
    Nothing  -> pure ()
    Just eff -> modify $ \(runtime :: Runtime ls t) ->
                           runtime&singular (signalAtDyn signal).subscribers %~ Set.insert eff
  where
    current'' = coerce current' :: Maybe (RegisteredEffect t Dynamic.Dynamic)


-- -- | Modify a signal value.
-- modifySignal_              :: forall ls t es a. ( HasRuntime ls t :> es
--                                                 , Subset ls es
--                                                 , Typeable a
--                                                 )
--                            => Ctx ls t -> Signal t a -> (a -> a) -> Eff es ()
-- modifySignal_ ctx signal f = void $ modifySignal ctx signal f

--------------------------------------------------------------------------------

-- | Run some effectful computation using a local state.
withLocalState       :: (State s :> es)
                     => (s -> (old, s))
                     -- ^ function to re-initialize the state
                     -> (old -> s -> s)
                     -- ^ function that recombines the previous state and the newer
                     -- state (i.e. after running the action) into a final state
                     -> (old -> Eff es a)
                     -- ^ The effect to run with the local state. Also still has access
                     -- to the old state
                     -> Eff es a
withLocalState initialize recombine act = do old  <- state initialize
                                             x    <- act old
                                             modify $ recombine old
                                             pure x


--------------------------------------------------------------------------------

-- | Create a new registered effect and run it.
createEffect          :: (HasRuntime ls t :> es, Subset ls es, Typeable r )
                      => Ctx ls t -> Eff (HasRuntime ls t : ls) r
                      -> Eff es (RegisteredEffect t r, r)
createEffect _ctx eff = do
    effIx <- registerEffect eff
    res   <- fromMaybe effectNoResultError <$> runEffect' effIx eff
    pure (effIx, res)

-- | Create a new registered effect and run it.
createEffect_         :: ( HasRuntime ls t :> es, Subset ls es)
                      => Ctx ls t -> Eff (HasRuntime ls t : ls) () -> Eff es ()
createEffect_ ctx eff = void $ createEffect ctx eff


-- | Runs the given given effect.
--
-- pre: the effect is in the system, and returns something of type r
runEffect         :: forall ls t es r.
                     ( HasRuntime ls t :> es
                     , Subset ls es
                     , Typeable r
                     )
                  => Ctx ls t -> RegisteredEffect t r -> Eff es r
runEffect _ effIx = do (eff :: Eff (HasRuntime ls t:ls) r) <- gets (^.effectAt' effIx)
                       fromMaybe effectNoResultError <$> runEffect' @ls effIx eff

-- | helper
effectNoResultError :: r
effectNoResultError = error "runEffect: absurd. No result!?"

-- | the actual implementation of runEffect (i.e. given both the
-- effectIx as well as the effect).
runEffect'           :: forall ls t es r.
                        ( HasRuntime ls t :> es
                        , Subset ls es
                        )
                     => RegisteredEffect t r
                     -> Eff (HasRuntime ls t : ls) r
                     -> Eff es (Maybe r)
runEffect' effIx eff = withLocalState (currentEffect %%~ \oldIx -> (oldIx, newEffIx))
                                      recombine  -- we restore only the currentEffect
                                      (\oldIx -> if oldIx /= newEffIx
                                                 then Just <$> inject eff else pure Nothing
                                      )
                                      -- don't run the effect if we
                                      -- are currently already running
                                      -- the effect.
  where
    newEffIx = coerce $ Just effIx
    recombine                :: Maybe (RegisteredEffect t Dynamic.Dynamic)
                             -> Runtime ls t -> Runtime ls t
    recombine oldIx newState = newState&currentEffect .~ oldIx




-- | Create/register a new registered effect.
registerEffect   :: forall ls t es r.
                    (HasRuntime ls t :> es, Typeable r )
                 => Eff (HasRuntime ls t : ls) r
                 -> Eff es (RegisteredEffect t r)
registerEffect f = state $ \(runtime :: Runtime ls t) ->
                             let i = runtime^.nextEffectId
                             in ( RegisteredEffect i :: RegisteredEffect t r
                                , runtime&nextEffectId %~ succ
                                         &rawEffects   %~ IntMap.insert i (Dynamic.toDyn <$> f)
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


--------------------------------------------------------------------------------

-- | Class for signal like things for which we can get the current value.
class HasCurrent ls t es signal a where
  -- | Access the current value of a signal (or a derived signal, or varying).
  current :: Ctx ls t -> signal t a -> Eff es a

instance (Typeable a, HasRuntime ls t :> es) => HasCurrent ls t es Signal a where
  current = getSignal


newtype Constant t a = Constant { getConstant :: a }
  deriving stock (Show,Eq,Ord,Functor,Foldable,Traversable)
  deriving (Applicative, Monad) via Identity

instance HasCurrent ls t es Constant a where
  current _ (Constant x) = pure x
