-- Five sites do private work and also bump one shared counter.
-- Expected: HOT SCALAR on the counter.
import Control.Concurrent.STM
import Control.Monad (forM)
import Fixture

main :: IO ()
main = do
  counter <- newTVarIO (0 :: Int)
  own <- forM [1 .. 5 :: Int] newTVarIO
  let step v = atomically $ do
        x <- readTVar v
        writeTVar v $! spin 300 x
        modifyTVar' counter (+ 1)
  runFor 1 (map step own)
