-- One TVar holds a record. Writers only ever touch `jobs`; the victim only
-- reads `config` and `sessions`. Expected: SPLIT, naming the jobs word.
import Control.Concurrent.STM
import Data.Map.Strict qualified as M
import Fixture

data App = App { jobs :: [Int], config :: M.Map Int Int, sessions :: M.Map Int Int }

main :: IO ()
main = do
  app <- newTVarIO (App [] (M.fromList [(i, i) | i <- [1 .. 64]]) (M.fromList [(i, 2 * i) | i <- [1 .. 64]]))
  out <- newTVarIO 0
  let enqueue n = atomically $ modifyTVar' app (\a -> a { jobs = n : take 8 (jobs a) })
      handle = atomically $ do
        a <- readTVar app
        let !r = spin 2000 (sum (config a) + sum (sessions a))
        writeTVar out r
  runFor 1 (handle : handle : map enqueue [1, 2])
