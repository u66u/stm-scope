-- | Plain-text report: ranked findings first, then the per-site table, then
-- anything that makes the numbers less trustworthy.
module StmScope.Report
  ( render, diff, siteNames, label, advice, diagnostics, tvarName, changes, readsText, unqual, duration
  ) where

import Data.Bits (testBit, (.|.))
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showFFloat, showHex)

import StmScope.Analyze
import StmScope.Facts
import StmScope.Verdict
import StmScope.Wire

render :: Int -> Profile -> Text
render top p = T.unlines . L.intercalate [""] . filter (not . null) $
  [header, L.intercalate [""] (zipWith finding [1 ..] shown) <> omitted, siteTable, escalation, diagnostics p]
  where
    -- below 1% of the wasted time a finding is noise, unless it is the only news
    (shown, rest) = splitAt top [f | f <- findings p, fdSaving f >= 0.01 * wasted || fdVerdict f `elem` [Herd, ByDesign]]
    small   = [f | f <- findings p, fdSaving f > 0, fdSaving f < 0.01 * wasted] <> rest
    omitted = [ "… and " <> plural (length small) "smaller finding" <> ", " <> pct (sum (map fdSaving small)) wasted
                  <> " of the wasted time together" | not (null small) ]
    sites   = pSites p
    names   = siteNames p
    name s  = IM.findWithDefault ("site " <> tshow s) s names
    total f = sum (map (f . siStats) (IM.elems sites))
    wasted  = fromIntegral (total sWasted) :: Double
    work    = wasted + fromIntegral (total sLatency)

    header =
      [ "stm-scope  ·  run " <> duration (fromIntegral (pDuration p)) <> "  ·  "
          <> count (total sCommits) <> " commits  ·  " <> count (total sFails) <> " failed attempts"
      , "wasted " <> duration wasted <> " of STM work (" <> pct wasted work <> ")  ·  "
          <> count (IM.size sites) <> " sites  ·  " <> count (IM.size (pWatches p)) <> " watched TVars"
      ]

    finding :: Int -> Finding -> [Text]
    finding i f@Finding {..} =
      [ T.justifyLeft 4 ' ' ("#" <> tshow i) <> T.justifyLeft 17 ' ' (label fdVerdict) <> cost
      , "    victim  " <> name fdVictim ]
      <> [ "    TVar    " <> tvName <> tvar | fdTVar /= 0 ]
      <> [ "    reads   " <> readsText fdLayout r | Just r <- [fdReads], edgeLike ]
      <> [ "    TVars   " <> count fdTVars <> " different ones, none with half the cost" | fdTVars > 1, edgeLike ]
      <> (if edgeLike then map writer (take 4 fdWriters) else [])
      <> [ "    … and " <> plural (length fdWriters - 4) "more writer" | length fdWriters > 4, edgeLike ]
      <> [ "    → " <> advice p f ]
      where
        cost | fdVerdict == Herd = "est. " <> duration fdSaving <> " of re-runs and failed parks"
             | fdSaving > 0 = pct fdSaving wasted <> " of wasted  ·  " <> duration fdSaving
                              <> (if attempts > 0 then "  ·  " <> plural attempts "failed attempt" else "")
             | otherwise    = "costs nothing"
        edgeLike = fdVerdict `notElem` [Herd, ByDesign, Unattributed]
        -- an attempt with many culprits counts once per culprit on edges
        attempts | fdTVars > 1 = maybe 0 (sFails . siStats) (IM.lookup fdVictim sites)
                 | otherwise   = esFails fdStat
        arity = maybe 0 pArity fdValue
        tvName = tvarName p f
        tvar = case fdValue of
          Nothing -> ""
          Just v  -> (if T.null (pType v) then "" else " :: " <> pType v) <> ", holding " <> unqual (pCon v)
            <> (if arity > 0 then " (" <> count arity <> " word" <> (if arity == 1 then ")" else "s)") else "")
            <> built
        built = case L.nub [loc v | Just v <- map (`M.lookup` pProv p) (S.toList (esValues fdStat)), not (T.null (pFile v))] of
          []   -> ""
          locs -> ", values built at " <> T.intercalate ", " (take 3 locs)
        writer (Writer s st) =
          "    writer  " <> pad 9 (count (esFails st)) <> "  "
            <> (if s == 0 then "(before the TVar was watched)" else name s)
            <> T.concat ["  ·  " <> c | let c = changes fdLayout arity st, not (T.null c)]


    siteTable = ("    " <> row ["commits", "fails", "wasted", "p50", "p99"] <> "  site") : concatMap line ranked
      where
        ranked = L.sortOn (Down . sWasted . siStats . snd) [x | x@(_, si) <- IM.toList sites, sCommits (siStats si) + sFails (siStats si) > 0]
        row = T.concat . map (pad 10)
        line (s, si) =
          let st = siStats si
              q f = maybe "-" (duration . fromIntegral . f) (siTail si)
          in ("    " <> row [count (sCommits st), count (sFails st), duration (fromIntegral (sWasted st)), q tP50, q tP99]
                <> "  " <> name s)
             : [ T.replicate 56 " " <> t | Just t <- [tailText =<< siTail si] ]
    tailText Tail {..}
      | tRetried == 0 && tBlocked == 0 = Nothing
      | otherwise = Just $ "p99 and up: " <> count tCount <> " calls, " <> count tRetried <> " retried, "
          <> pct (fromIntegral tWasted) tLatency <> " of their time wasted"
          <> (if tBlocked > 0 then ", blocked " <> duration (fromIntegral tBlocked) else "")
          <> (if tGC > 0 then ", " <> count tGC <> " overlapped a GC" else "")

    escalation
      | null (pEscalate p) = []
      | otherwise =
          [ "escalation: " <> count (length [() | MEscalate {} <- pEscalate p]) <> " grants; released "
              <> T.intercalate ", " [tshow r <> " " <> count n | (r, n) <- M.toList releases]
              <> "  ·  " <> count (total sVetoed) <> " commits vetoed" ]
    releases = M.fromListWith (+) [(r, 1 :: Int) | MRelease _ r <- pEscalate p]

-- | A finding's TVar: the name both sides use for it, else the victim's
-- candidates, else its watch number.
tvarName :: Profile -> Finding -> Text
tvarName p Finding {..} = case cands [fdVictim] of
  [] -> "#" <> T.pack (show fdTVar)
  vs -> case cands (fdVictim : [s | Writer s _ <- fdWriters, s /= 0]) of
    [(n, l)] -> n <> " (" <> l <> ")"
    _        -> T.intercalate " or " (map fst (take 3 vs))
  where
    cands ss = tvarNames (pFacts p) ss holds
    -- the contents' type: from the layout, else any type of the constructor's module
    holds tc = case (fdLayout, fdValue) of
      (Just l, _) -> tc == lTyCon l
      (_, Just v) | m <- fst (T.breakOnEnd "." (pCon v)), not (T.null m), Just t <- T.stripPrefix m tc -> not (T.any (== '.') t)
      _ -> False

-- | Anything that makes the numbers less trustworthy; lines starting with
-- '!' are warnings.
diagnostics :: Profile -> [Text]
diagnostics p = concat
  [ [ "! no exit summary: the program did not shut down normally; numbers are partial" | Nothing <- [pSummary p] ]
  , [ "! eventlog truncated: " <> T.pack e | Just e <- [pTruncated p] ]
  , [ "! " <> count (length (pBad p)) <> " malformed stm-scope messages (shim/CLI version mismatch?): " <> T.pack e | e : _ <- [pBad p] ]
  , concat [ dropped z | Just z <- [pSummary p] ]
  ]
  where
    dropped Summary {..} = concat
      [ [ "! " <> tshow zDroppedTx <> " calls untracked (thread table full)" | zDroppedTx > 0 ]
      , [ "! " <> tshow zDroppedSites <> " calls at unknown sites (site table full)" | zDroppedSites > 0 ]
      , [ "! " <> tshow zDroppedWatch <> " times a culprit TVar could not be watched (watch table full); its writers are unknown" | zDroppedWatch > 0 ]
      , [ "! " <> tshow zDroppedEdges <> " failed attempts not recorded (out of memory)" | zDroppedEdges > 0 ]
      , [ "! canary: " <> tshow zCanaryBad <> " of " <> tshow zCanaryChecked
            <> " commits saw their TRec change under them; culprit data is unreliable on this GHC" | zCanaryBad > 0 ]
      , [ "canary: " <> count (fromIntegral zCanaryChecked) <> " commits checked, all intact" | zCanaryChecked > 0, zCanaryBad == 0 ]
      ]

-- | From run A to run B. Sites are matched by name and findings by verdict
-- and victim, so a site whose source span moved shows as gone and new.
diff :: Profile -> Profile -> Text
diff a b = T.unlines . L.intercalate [""] . filter (not . null) $ [warnings, header, table, changed]
  where
    warnings = map ("! " <>) $ concat
      [ [ "different RTS: " <> x <> " vs " <> y | Just x <- [pRts a], Just y <- [pRts b], x /= y ]
      , [ "--depth " <> tshow x <> " vs " <> tshow y <> ": sites are named by different caller chains"
        | Just x <- [zDepth <$> pSummary a], Just y <- [zDepth <$> pSummary b], x /= y ]
      , [ "--escalate " <> tshow x <> " vs " <> tshow y <> ": B ran under different scheduling"
        | Just x <- [zEscalate <$> pSummary a], Just y <- [zEscalate <$> pSummary b], x /= y ] ]
      <> [ "run " <> r <> ": " <> T.drop 2 d | (r, q) <- [("A", a), ("B", b)], d <- diagnostics q, "! " `T.isPrefixOf` d ]
    header =
      [ "stm-scope diff  ·  A " <> duration (fromIntegral (pDuration a)) <> "  ·  B " <> duration (fromIntegral (pDuration b))
      , "wasted " <> change duration (wastedOf a) (wastedOf b) <> "  ·  commits/s " <> change rate (commitRate a) (commitRate b) ]
    wastedOf q = fromIntegral (sum (sWasted . siStats <$> IM.elems (pSites q)))
    commitRate q = perSecond q (fromIntegral (sum (sCommits . siStats <$> IM.elems (pSites q))))

    table = ("    " <> cell "wasted" <> cell "p99" <> cell "commits/s" <> "site")
      : [ "    " <> cell (col (fromIntegral . sWasted) duration) <> cell (col p99 duration)
            <> cell (colP (\q -> perSecond q . fromIntegral . sCommits) rate) <> n
        | (n, x, y) <- sortedSites
        , let col f sh = side sh (f <$> x) (f <$> y)
              colP f sh = side sh (f a <$> x) (f b <$> y) ]
    sortedSites = L.sortOn (\(_, x, y) -> Down (max (w x) (w y)))
      [ (n, M.lookup n sa, M.lookup n sb) | n <- S.toList (M.keysSet sa <> M.keysSet sb) ]
      where w = maybe 0 sWasted
    sa = sitesOf a
    sb = sitesOf b
    sitesOf q = M.fromListWith (<>) [(IM.findWithDefault "" s ns, siStats si) | let ns = siteNames q, (s, si) <- IM.toList (pSites q)]
    p99 = fromIntegral . quantile 0.99 . fmap bCalls . sHist

    changed = case [ line k x y | (k, (x, y)) <- M.toList (M.unionWith both (one fa) (two fb)), significant x y ] of
      [] -> []
      ls -> "findings" : ls
      where
        both (x, _) (_, y) = (x, y)
        one = fmap (\v -> (v, 0)); two = fmap (\v -> (0, v))
        fa = findingsOf a; fb = findingsOf b
        significant x y = max x y >= 0.01 * max (wastedOf a) (wastedOf b)
        line (v, n) x y = "    " <> T.justifyLeft 6 ' ' (if x == 0 then "new" else if y == 0 then "gone" else "")
          <> T.justifyLeft 17 ' ' (label v) <> cell (change duration x y) <> n
    findingsOf q = M.fromListWith (+) [((fdVerdict f, IM.findWithDefault "" (fdVictim f) ns), fdSaving f) | let ns = siteNames q, f <- findings q]

    side sh x y = case (x, y) of
      (Just u, Just v) -> change sh u v
      (Just u, Nothing) -> sh u <> " → gone"
      (Nothing, Just v) -> "new → " <> sh v
      _ -> ""
    change sh x y = sh x <> " → " <> sh y <> (if x > 0 then " (" <> signed (100 * (y - x) / x) <> ")" else "")
    signed d = (if d >= 0 then "+" else "") <> fixed 0 d <> "%"
    rate r = count (round r)
    cell t = T.justifyLeft 30 ' ' t <> "  "

label :: Verdict -> Text
label = \case
  Split -> "SPLIT"; HotScalar -> "HOT SCALAR"; LongVictim -> "LONG VICTIM"
  TrueConflict -> "TRUE CONFLICT"; AbaChurn -> "ABA CHURN"; LockCollision -> "LOCK COLLISION"
  Herd -> "THUNDERING HERD"; ByDesign -> "RETRY BY DESIGN"; Unattributed -> "UNATTRIBUTED"

advice :: Profile -> Finding -> Text
advice p Finding {..} = case fdVerdict of
  Split -> "split " <> con <> ": move " <> words' fdLayout changed <> " into a TVar of its own; these writers change nothing else"
    <> maybe ". If the victim reads it too, this is a true conflict" (const ", and the victim does not read it") fdReads
  HotScalar -> "one word written by " <> plural (length known) "site" <> " at " <> rate
    <> ": stripe or shard it, or move it out of the transaction (IORef, atomicModifyIORef') if it need not be atomic with the rest"
  LongVictim -> "mean failed attempt " <> duration median <> ", a conflicting write every " <> duration (1e9 / max 1e-9 writerRate)
    <> ": move pure work out of the transaction, read a snapshot with readTVarIO, or try --escalate"
  TrueConflict
    | Just r <- fdReads, Just l <- fdLayout, esCon fdStat == 0, esMask fdStat /= 0 ->
        (case r of Whole -> "the victim may read all of it"
                   Fields fs -> "the victim reads " <> set (S.intersection fs (fieldsOf l changed)))
        <> ", which these writers change: a split cannot help; shorten the transactions or funnel the writers through one place"
    | esFields fdStat > 0, esMask fdStat == 0, esCon fdStat == 0 ->
        "writers store a value equal to the one already there: compare before writing, and skip the write"
    | otherwise -> "the same data is contended: shorten the transactions or funnel the writers through one place"
  AbaChurn -> "a TVar in the read set changed and changed back: make it monotonic (a counter or epoch) or give the flag its own TVar"
  LockCollision -> "lost to commits in progress, not to new values: a commit-time locking artifact of the RTS; shorter writers help"
  Herd -> count (sWaits st) <> " waits for " <> count (sCommits st)
    <> " commits, woken waiters mostly find nothing to do: give each waiter its own TVar, or hand off through a TMVar or TQueue"
  ByDesign -> "blocked " <> duration (fromIntegral (sBlocked st)) <> " in retry and rarely in conflict: waiting by design, not contention"
  Unattributed -> "no recorded writer: an unexplained read-set change, a culprit beyond the first 8, or a full watch table"
  where
    known = filter ((/= 0) . wSite) fdWriters
    changed = foldr ((.|.) . esMask . wStat) 0 known
    con = maybe "the record" (unqual . pCon) fdValue
    rate = count (round (writeRate p fdTVar)) <> " writes/s"
    st = maybe mempty siStats (IM.lookup fdVictim (pSites p))
    median = maybe 0 meanAttempt (IM.lookup fdVictim (pSites p))
    -- one TVar: its write rate; spread over many: the writers' commit rate
    writerRate
      | fdTVar /= 0 = writeRate p fdTVar
      | otherwise = sum [perSecond p (fromIntegral (sCommits (siStats si))) | Writer s _ <- known, Just si <- [IM.lookup s (pSites p)]]

changes :: Maybe Layout -> Int -> EdgeStat -> Text
changes lay arity st
  | esLocked st > 0 && esFields st + esCon st == 0 = "was committing (TVar locked)"
  | esCon st > 0 && esFields st == 0              = "replaces the constructor"
  | esFields st > 0, esMask st == 0               = "stores an equal value"
  | esFields st > 0, Just _ <- lay                = "changes " <> words' lay (esMask st)
  | esFields st > 0, arity > 1                    = "changes " <> words' lay (esMask st) <> " of " <> tshow arity
  | otherwise                                     = ""

-- | Source fields if the build's layout is known; else payload words (GHC
-- lays out pointers first, then unboxed fields, so these are not field numbers).
words' :: Maybe Layout -> Word64 -> Text
words' (Just l) m = set (fieldsOf l m)
words' Nothing m = "payload word" <> (if length bs == 1 then " " else "s ") <> T.intercalate "," (map tshow bs)
  where bs = [i | i <- [0 .. 63 :: Int], testBit m i]

set :: S.Set Text -> Text
set fs = "{" <> T.intercalate ", " (S.toList fs) <> "}"

readsText :: Maybe Layout -> Reads -> Text
readsText _ Whole = "the victim may read all of it (passes it on, or uses it where the analysis cannot follow)"
readsText _ (Fields fs)
  | S.null fs = "the victim reads none of its fields"
  | otherwise = "the victim reads " <> set fs

-- | "label  file:span  ←  caller": the body, then as many located callers as
-- it takes to tell the site from every other one (sites differ in some
-- frame, but a shared helper such as a logging wrapper can hide which).
siteNames :: Profile -> IM.IntMap Text
siteNames p = IM.insert 0 "(unknown site)" (IM.fromList (refine 1 (IM.keys (pSites p))))
  where
    refine k ss = concat
      [ if length g == 1 || k >= 6 then [(s, t) | s <- g] else refine (k + 1) g
      | (t, g) <- M.toList (M.fromListWith (flip (<>)) [(name k s, [s]) | s <- ss]) ]
    name k s = case mapMaybe (`M.lookup` pProv p) frames of
      body : callers -> T.intercalate "  ←  " (named body : take k (map named (filter located callers)))
      [] -> maybe ("site " <> tshow s) (("0x" <>) . T.pack . (`showHex` "")) (L.find (const True) frames)
      where frames = maybe [] siFrames (IM.lookup s (pSites p))
    located = not . T.null . pFile
    named v = fromMaybe "?" (L.find (not . T.null) [pLabel v, pTable v]) <> if located v then "  " <> loc v else ""

loc :: Prov -> Text
loc v = pFile v <> ":" <> pSpan v

-- | Constructor descriptions are "package:Module.Con": drop the package.
unqual :: Text -> Text
unqual t = case T.breakOn ":" t of
  (_, rest) | not (T.null rest) -> T.drop 1 rest
  _ -> t

------------------------------------------------------------------------------
-- Numbers

duration :: Double -> Text
duration ns
  | ns < 1e3  = fixed 0 ns <> " ns"
  | ns < 1e6  = fixed 1 (ns / 1e3) <> " µs"
  | ns < 1e9  = fixed 1 (ns / 1e6) <> " ms"
  | otherwise = fixed 2 (ns / 1e9) <> " s"

pct :: Double -> Double -> Text
pct _ 0 = "-"
pct a b = fixed 0 (100 * a / b) <> "%"

fixed :: Int -> Double -> Text
fixed d x = T.pack (showFFloat (Just d) x "")

count :: Int -> Text
count n | n < 0 = "-" <> count (negate n)
        | otherwise = T.intercalate "," . reverse . map T.reverse . T.chunksOf 3 . T.reverse $ tshow n

plural :: Int -> Text -> Text
plural n w = count n <> " " <> w <> if n == 1 then "" else "s"

tshow :: Show a => a -> Text
tshow = T.pack . show

pad :: Int -> Text -> Text
pad = (`T.justifyRight` ' ')
