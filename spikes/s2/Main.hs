import Control.Concurrent.STM
import Control.Monad

bump :: TVar Int -> STM ()
bump tv = modifyTVar' tv (+1)

{-# NOINLINE helper #-}
helper :: TVar Int -> IO Int
helper tv = do
  atomically (bump tv)
  n <- readTVarIO tv
  pure (n + 1)

main :: IO ()
main = do
  tv <- newTVarIO (0 :: Int)
  xs <- replicateM 3 (helper tv)
  print xs
