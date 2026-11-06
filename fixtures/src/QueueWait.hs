-- One consumer blocks on a queue that a slow producer fills.
-- Expected: RETRY BY DESIGN, and nothing worse.
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Fixture

main :: IO ()
main = do
  q <- newTQueueIO
  sink <- newTVarIO (0 :: Int)
  let produce = threadDelay 1000 >> atomically (writeTQueue q (1 :: Int))
      consume = atomically $ readTQueue q >>= \x -> modifyTVar' sink (+ x)
  runFor 1 [produce, consume]
