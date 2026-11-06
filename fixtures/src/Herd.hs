-- 32 threads wait for their turn on one TVar; every turn wakes all of them.
-- Expected: THUNDERING HERD.
import Control.Concurrent.STM
import Fixture

main :: IO ()
main = do
  turn <- newTVarIO (0 :: Int)
  let k = 32
      waiter i = atomically $ do
        t <- readTVar turn
        check (t `mod` k == i)
        writeTVar turn (t + 1)
  runFor 1 (map waiter [0 .. k - 1])
