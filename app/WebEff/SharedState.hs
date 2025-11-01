-- | This essentially a copy of Effectful.State.Static.Shared that also allows
-- us to access the actual MVar
module WebEff.SharedState
  ( -- * Effect
    State

    -- ** Handlers
  , runState
  , evalState
  , execState

  , runStateMVar
  , evalStateMVar
  , execStateMVar

  -- **
  , getStateMVar

    -- ** Operations
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Control.Concurrent.MVar.Strict
import Data.Kind

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Provide access to a strict (WHNF), shared, mutable value of type @s@.
data State (s :: Type) :: Effect

type instance DispatchOf (State s) = Static NoSideEffects
newtype instance StaticRep (State s) = State (MVar' s)

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state.
runState :: HasCallStack => s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  v <- unsafeEff_ $ newMVar' s
  a <- evalStaticRep (State v) m
  (a, ) <$> unsafeEff_ (readMVar' v)

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state.
evalState :: HasCallStack => s -> Eff (State s : es) a -> Eff es a
evalState s m = do
  v <- unsafeEff_ $ newMVar' s
  evalStaticRep (State v) m

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value.
execState :: HasCallStack => s -> Eff (State s : es) a -> Eff es s
execState s m = do
  v <- unsafeEff_ $ newMVar' s
  _ <- evalStaticRep (State v) m
  unsafeEff_ $ readMVar' v

-- | Run the 'State' effect with the given initial state 'MVar'' and return the
-- final value along with the final state.
runStateMVar :: HasCallStack => MVar' s -> Eff (State s : es) a -> Eff es (a, s)
runStateMVar v m = do
  a <- evalStaticRep (State v) m
  (a, ) <$> unsafeEff_ (readMVar' v)

-- | Run the 'State' effect with the given initial state 'MVar'' and return the
-- final value, discarding the final state.
evalStateMVar :: HasCallStack => MVar' s -> Eff (State s : es) a -> Eff es a
evalStateMVar v = evalStaticRep (State v)

-- | Run the 'State' effect with the given initial state 'MVar'' and return the
-- final state, discarding the final value.
execStateMVar :: HasCallStack => MVar' s -> Eff (State s : es) a -> Eff es s
execStateMVar v m = do
  _ <- evalStaticRep (State v) m
  unsafeEff_ $ readMVar' v

-- | Access the underlying MVar'
getStateMVar :: (HasCallStack, State s :> es) => Eff es (MVar' s)
getStateMVar = unsafeEff $ \es -> do
  State v <- getEnv es
  pure v

-- | Fetch the current value of the state.
get :: (HasCallStack, State s :> es) => Eff es s
get = unsafeEff $ \es -> do
  State v <- getEnv es
  readMVar' v

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets :: (HasCallStack, State s :> es) => (s -> a) -> Eff es a
gets f = f <$> get

-- | Set the current state to the given value.
put :: (HasCallStack, State s :> es) => s -> Eff es ()
put s = unsafeEff $ \es -> do
  State v <- getEnv es
  modifyMVar'_ v $ \_ -> pure s

-- | Apply the function to the current state and return a value.
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
state :: (HasCallStack, State s :> es) => (s -> (a, s)) -> Eff es a
state f = unsafeEff $ \es -> do
  State v <- getEnv es
  modifyMVar' v $ \s0 -> let (a, s) = f s0 in pure (s, a)

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
modify :: (HasCallStack, State s :> es) => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

-- | Apply the monadic function to the current state and return a value.
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
stateM :: (HasCallStack, State s :> es) => (s -> Eff es (a, s)) -> Eff es a
stateM f = unsafeEff $ \es -> do
  State v <- getEnv es
  modifyMVar' v $ \s0 -> do
    (a, s) <- unEff (f s0) es
    pure (s, a)

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
modifyM :: (HasCallStack, State s :> es) => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
