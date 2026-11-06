-- | Run workers in a loop for a fixed time, then exit.
module Fixture (runFor, spin) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_)

runFor :: Double -> [IO ()] -> IO ()
runFor secs workers = do
  forM_ workers (forkIO . forever)
  threadDelay (round (secs * 1e6))

-- | Pure work that GHC cannot float out: n rounds of an LCG from x.
spin :: Int -> Int -> Int
spin n x = go n x where
  go 0 !a = a
  go k !a = go (k - 1) (a * 6364136223846793005 + 1442695040888963407)
