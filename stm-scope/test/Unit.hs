-- Wire decoder and histogram (where Haskell must agree with C), the facts
-- protocol (where it must agree with the helper), and JSON escaping.
module Main (main) where

import Control.Monad (unless)
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Exit (exitFailure)

import StmScope.Facts
import StmScope.Json (J (..), encode)
import StmScope.Wire

main :: IO ()
main = do
  let checks =
        [ ("bucket . bucketLow == id", all (\b -> bucket (bucketLow b) == b) [0 .. 495])
        , ("bucketLow is monotone", and (zipWith (<) (map bucketLow [0 .. 494]) (map bucketLow [1 .. 495])))
        , ("buckets cover 2^63", bucket maxBound == 495)
        , ("bucket edges", all (\n -> bucketLow (bucket n) <= n && (bucket n == 495 || n < bucketLow (bucket n + 1)))
                               ([0 .. 4096] <> concat [[2 ^ k - 1, 2 ^ k, 2 ^ k + 1] | k <- [12 .. 62 :: Int]]))
        , ("quantile", quantile 0.5 (IM.fromList [(bucket 10, 1), (bucket 100, 1), (bucket 1000, 1)]) == bucketLow (bucket 100))
        , ("decode G", decode edge == Just (Right (MEdge (EdgeMsg 3 5 4 Conflict 9 7 1 1 0 900 6 0xa))))
        , ("decode I", decode info == Just (Right (MInfo 0x401000 (Prov 2 1 "main:Main.App" "" "App" "" "Main" "src/A.hs" "3:5-9"))))
        , ("parseFacts", parseFacts facts == Facts (IM.fromList [(7, "main")])
              (M.fromList [(("main", "main:Main.App"), Layout "Main.App" 2 1 ["jobs", "config", "n"])])
              (M.fromList [((7, "Main.App"), Fields (S.fromList ["config"])), ((8, "Main.B"), Whole)])
              (M.fromList [((7, "Main.App"), [("app", "src/A.hs:3")])]))
        , ("fieldsOf", fieldsOf (Layout "T" 2 1 ["a", "b", "c"]) 0b101 == S.fromList ["a", "c"])
        , ("json escapes", encode (O [("k\"", A [S "a\\b\n\1", N 2, N 0.5, B True, Null])]) == "{\"k\\\"\":[\"a\\\\b\\n\\u0001\",2,0.5,true,null]}")
        , ("decode foreign payload", decode "hello" == Nothing)
        , ("decode truncated", isLeft' (decode (BL.toStrict (BL.take 12 (BL.fromStrict edge)))))
        ]
      edge = BL.toStrict . runPut $ do
        putWord8 0x53; putWord8 0x47
        mapM_ putWord32le [3, 5, 4]; putWord8 1
        mapM_ putWord32le [9, 7, 1, 1, 0]; mapM_ putWord64le [900, 6, 0xa]
      info = BL.toStrict . runPut $ do
        putWord8 0x53; putWord8 0x49; putWord64le 0x401000; putWord16le 2; putWord16le 1
        mapM_ (\t -> putLazyByteString (BC.pack t) >> putWord8 0) ["main:Main.App", "", "App", "", "Main", "src/A.hs", "3:5-9"]
      facts = "comp\t7\tmain\ncon\tmain\tmain:Main.App\tMain.App\t2\t1\tjobs,config,n\nread\t7\tMain.App\tconfig\n"
        <> "read\t8\tMain.B\t*\ntvar\t7\tMain.App\tapp\tsrc/A.hs:3\nmiss\t9\n"
      isLeft' = \case Just (Left _) -> True; _ -> False
      bad = [name | (name, False) <- checks]
  mapM_ (putStrLn . ("FAIL " <>)) bad
  unless (null bad) exitFailure
  putStrLn ("ok, " <> show (length checks) <> " checks")
