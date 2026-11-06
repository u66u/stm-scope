-- A report sums 2000 TVars in one transaction while writers update random
-- ones. Expected: LONG VICTIM, spread over many TVars.
import Control.Concurrent.STM
import Control.Monad (forM, replicateM)
import Data.IORef
import Fixture

main :: IO ()
main = do
  cells <- replicateM 2000 (newTVarIO (0 :: Int))
  out <- newTVarIO 0
  seeds <- forM [1 .. 3 :: Int] newIORef
  let n = length cells
      writer seed = do
        s <- atomicModifyIORef' seed (\x -> let y = spin 1 x in (y, y))
        atomically $ modifyTVar' (cells !! (abs s `mod` n)) (+ 1)
      report = atomically $ do
        xs <- mapM readTVar cells
        writeTVar out $! sum xs
  runFor 1 (report : map writer seeds)
