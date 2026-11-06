import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

main :: IO ()
main = do
  tv <- newTVarIO (0 :: Int)
  done <- newEmptyMVar
  forM_ [1..4::Int] $ \_ -> forkIO $ do
    replicateM_ 25000 (atomically (modifyTVar' tv (+1)))
    putMVar done ()
  replicateM_ 4 (takeMVar done)
  readTVarIO tv >>= print
