-- The long-reader pattern with a fixed amount of work: three writers move a
-- unit between two random cells, 200,000 times each, so the cells always sum
-- to 0; a reader takes 50 snapshots. Without escalation the reader starves
-- until the writers are done. With --escalate it gets through, every
-- snapshot still sums to 0, and the final state is the same: increments
-- commute, so it does not depend on the interleaving.
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array
import Fixture

main :: IO ()
main = do
  let n = 2000
  cells <- listArray (0, n - 1) <$> replicateM n (newTVarIO (0 :: Int))
  done <- newEmptyMVar
  let writer seed = do
        let go 0 _ = pure ()
            go k s = do
              let s' = spin 1 s
                  i = abs s' `mod` n
                  j = abs (s' `div` 7) `mod` n
              atomically $ modifyTVar' (cells ! i) (+ 1) >> modifyTVar' (cells ! j) (subtract 1)
              go (k - 1 :: Int) s'
        go 200000 seed >> putMVar done ()
      snapshot = atomically (sum <$> mapM readTVar (elems cells))
  forM_ [1 .. 3] (forkIO . writer)
  sums <- replicateM 50 snapshot
  replicateM_ 3 (takeMVar done)
  final <- mapM readTVarIO (elems cells)
  putStrLn ("snapshots " <> (if all (== 0) sums then "consistent" else "BROKEN " <> show sums)
            <> ", state " <> show (sum (zipWith (*) [1 ..] final)))
