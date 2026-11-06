-- A writer raises a flag and lowers it again in two transactions, while a
-- slow reader depends on it. The reader keeps failing although the value it
-- read is back when it commits. Expected: ABA CHURN on the flag.
import Control.Concurrent.STM
import Fixture

main :: IO ()
main = do
  flag <- newTVarIO False
  out <- newTVarIO 0
  let toggle = atomically (writeTVar flag True) >> atomically (writeTVar flag False)
      reader = atomically $ do
        f <- readTVar flag
        writeTVar out $! spin 20000 (fromEnum f)
  runFor 1 [reader, toggle]
