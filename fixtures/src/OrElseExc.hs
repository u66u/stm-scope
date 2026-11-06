-- Branch switching in orElse, exceptions thrown from STM, and threads killed
-- in the middle of a transaction, by the hundred thousand. Expected: no
-- finding about any of it, and no lost bookkeeping (no '!' lines).
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Fixture

main :: IO ()
main = do
  a <- newTVarIO (0 :: Int)
  b <- newTVarIO (0 :: Int)
  let pick = atomically $ (readTVar a >>= check . odd >> modifyTVar' a (+ 1)) `orElse` modifyTVar' b (+ 1)
      bump = atomically (modifyTVar' a (+ 1))
      boom = void (try @ArithException (atomically (readTVar b >>= \x -> when (even x) (throwSTM Overflow))))
      victim = do
        t <- forkIO (atomically (readTVar a >>= \x -> writeTVar b $! spin 100000 x))
        yield
        killThread t
  runFor 1 [pick, bump, boom, victim]
