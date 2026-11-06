-- Every transaction goes through one logging helper, and two callers run the
-- same STM action through it. Expected: two sites, told apart by caller.
import Control.Concurrent.STM
import Fixture

{-# NOINLINE atomicallyLogged #-}
atomicallyLogged :: STM a -> IO a
atomicallyLogged act = do
  r <- atomically act
  r `seq` pure r

main :: IO ()
main = do
  counter <- newTVarIO (0 :: Int)
  let bump = modifyTVar' counter (+ 1)
      producer = atomicallyLogged bump >> pure ()
      consumer = atomicallyLogged bump >>= \() -> pure ()
  runFor 1 [producer, consumer]
