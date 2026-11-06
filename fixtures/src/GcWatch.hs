-- S3: watches survive garbage collection. A writer bumps random cells of
-- 1000; a reader sums random windows of 50, so it fails on many different
-- cells; a third thread forces a major GC every millisecond, moving every
-- TVar. If the shim lost a TVar's identity when it moved, the TVar would be
-- watched again from scratch, and its next failures charged to no writer:
-- the unattributed failures would grow with the number of GCs, not with the
-- number of TVars.
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array
import Data.IORef
import System.Mem (performMajorGC)
import Fixture

main :: IO ()
main = do
  let n = 1000
  cells <- listArray (0, n - 1) <$> replicateM n (newTVarIO (0 :: Int))
  stop <- newIORef False
  gcs <- newIORef (0 :: Int)
  done <- newEmptyMVar
  let pick s = abs s `mod` n
      writer 0 _ = pure ()
      writer k s = do
        let s' = spin 1 s
        atomically $ modifyTVar' (cells ! pick s') (+ 1)
        writer (k - 1 :: Int) s'
      reader 0 _ acc = pure acc
      reader k s acc = do
        let s' = spin 1 s
            i = pick s'
        x <- atomically $ sum <$> mapM (readTVar . (cells !) . (`mod` n)) [i .. i + 49]
        reader (k - 1 :: Int) s' (acc + x)
      collector = readIORef stop >>= \case
        True -> pure ()
        False -> performMajorGC >> modifyIORef' gcs (+ 1) >> threadDelay 1000 >> collector
  _ <- forkIO (collector >> putMVar done ())
  w <- newEmptyMVar
  _ <- forkIO (writer 300000 1 >> putMVar w ())
  total <- reader 20000 2 0
  takeMVar w
  writeIORef stop True >> takeMVar done
  k <- readIORef gcs
  putStrLn ("gcs " <> show k <> ", " <> show (total >= 0))
