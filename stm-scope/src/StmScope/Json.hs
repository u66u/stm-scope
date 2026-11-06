-- | The report as JSON, for CI and scripts: every finding (not only the
-- ones the text report shows), every site, and the diagnostics. Durations
-- are in ns. Encoded by hand: the format is small and fixed.
module StmScope.Json (json, encode, J (..)) where

import Data.Char (ord)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

import StmScope.Analyze
import StmScope.Facts
import StmScope.Report
import StmScope.Verdict
import StmScope.Wire

data J = O [(Text, J)] | A [J] | S Text | N Double | B Bool | Null

encode :: J -> Text
encode = \case
  O kvs -> "{" <> T.intercalate "," [str k <> ":" <> encode v | (k, v) <- kvs] <> "}"
  A xs  -> "[" <> T.intercalate "," (map encode xs) <> "]"
  S t   -> str t
  N d | isNaN d || isInfinite d -> "null"
      | d == fromIntegral r -> T.pack (show r)
      | otherwise -> T.pack (show d)
      where r = round d :: Integer
  B b   -> if b then "true" else "false"
  Null  -> "null"
  where
    str t = "\"" <> T.concatMap esc t <> "\""
    esc = \case
      '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; '\t' -> "\\t"
      c | ord c < 0x20 -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
        | otherwise -> T.singleton c

json :: Profile -> Text
json p = encode $ O
  [ ("version", N 1)
  , ("rts", maybe Null S (pRts p))
  , ("duration_ns", n (pDuration p))
  , ("depth", maybe Null (n . zDepth) (pSummary p))
  , ("escalate", maybe Null (n . zEscalate) (pSummary p))
  , ("watched_tvars", n (IM.size (pWatches p)))
  , ("findings", A (map finding (findings p)))
  , ("sites", A [site s si | (s, si) <- IM.toList (pSites p)])
  , ("diagnostics", A (map S (diagnostics p)))
  ]
  where
    names = siteNames p
    name s = S (IM.findWithDefault "" s names)
    finding f@Finding {..} = O $
      [ ("verdict", S (label fdVerdict))
      , ("saving_ns", N fdSaving)
      , ("victim", name fdVictim)
      , ("tvar", if fdTVar == 0 then Null else O
          [ ("id", n fdTVar), ("name", S (tvarName p f))
          , ("type", maybe Null (S . pType) fdValue), ("constructor", maybe Null (S . unqual . pCon) fdValue) ])
      , ("reads", case fdReads of
          Nothing -> Null
          Just Whole -> S "*"
          Just (Fields fs) -> A (map S (foldr (:) [] fs)))
      , ("writers", A [ O [ ("site", if s == 0 then Null else name s), ("fails", n (esFails st))
                          , ("changes", S (changes fdLayout (maybe 0 pArity fdValue) st)) ]
                      | Writer s st <- fdWriters ])
      , ("advice", S (advice p f))
      ]
    site s SiteInfo {..} = O
      [ ("name", name s), ("commits", n (sCommits siStats)), ("fails", n (sFails siStats))
      , ("waits", n (sWaits siStats)), ("wasted_ns", n (sWasted siStats))
      , ("p50_ns", q tP50), ("p99_ns", q tP99), ("p999_ns", q tP999) ]
      where q f = maybe Null (n . f) siTail
    n :: Integral a => a -> J
    n = N . fromIntegral
