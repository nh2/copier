{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module PooledMapConcurrently
  ( pooledMapConcurrently
  , pooledMapConcurrently'
  ) where

import           Control.Concurrent.Async.Lifted (Concurrently(..))
import qualified Control.Concurrent.Async.Lifted.Safe as Async
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Data.IORef
import           Data.Foldable
import           Data.Traversable
import           GHC.Conc (getNumCapabilities)

pooledMapConcurrently :: (Traversable t, MonadIO m, Async.Forall (Async.Pure m), MonadBaseControl IO m) => (a -> m b) -> t a -> m (t b)
pooledMapConcurrently f xs = do
  numProcs <- liftIO getNumCapabilities
  pooledMapConcurrently' numProcs f xs

pooledMapConcurrently' :: forall t m a b . (Traversable t, MonadIO m, Async.Forall (Async.Pure m), MonadBaseControl IO m) => Int -> (a -> m b) -> t a -> m (t b)
pooledMapConcurrently' numThreads f xs = if numThreads < 1
 then error ("pooledMapConcurrently: numThreads < 1 (" ++ show numThreads ++ ")")
 else do

   jobs :: t (a, IORef b) <- liftIO $ for xs (\x -> (x, ) <$> newIORef (error "pooledMapConcurrently: empty IORef"))

   jobsVar :: MVar [(a, IORef b)] <- liftIO $ newMVar (toList jobs)

   runConcurrently $ for_ [1..numThreads] $ \_ -> Concurrently $ do
     let loop :: m ()
         loop = do
           m'job :: Maybe (a, IORef b) <- liftIO $ modifyMVar jobsVar $ \case
             [] -> return ([], Nothing)
             var : vars -> return (vars, Just var)
           for_ m'job $ \(x, outRef) -> do
             y <- f x
             liftIO $ atomicWriteIORef outRef y
             loop
     loop

   liftIO $ for jobs (\(_, outputRef) -> readIORef outputRef)
