-- | From a decoded eventlog to a 'Profile': who made whom fail, on which TVar,
-- changing which words, and what it cost.
--
-- The shim has already done the attribution (each failed attempt is charged
-- to the writer named in the culprit TVar's watch slot) and the aggregation,
-- so this is a fold: merge per-capability deltas, and merge sites the user
-- could not tell apart anyway.
--
-- A TVar is only watched after it first appears as a culprit, so its first
-- failures have no recorded writer: they show up with writer 0.
module StmScope.Analyze
  ( Profile (..), SiteInfo (..), Edge (..), EdgeStat (..), Tail (..)
  , analyze, writeRate, perSecond, meanAttempt
  ) where

import Data.Bits ((.|.))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.RTS.Events (Timestamp)

import StmScope.Facts (Facts, noFacts)
import StmScope.Wire

-- | Victim site, culprit TVar, writer site. 0 means unknown.
data Edge = Edge { eVictim :: !Site, eTVar :: !Watch, eAttacker :: !Site }
  deriving (Eq, Ord, Show)

data EdgeStat = EdgeStat
  { esFails  :: !Int         -- ^ failed attempts (one with two culprits counts on both edges)
  , esWasted :: !Double      -- ^ ns of discarded work, split over culprits
  , esMask   :: !Word64      -- ^ payload words seen changed
  , esFields, esCon, esOpaque, esLocked :: !Int
  , esValues :: !(Set Info)  -- ^ samples of the old values
  , esAba    :: !Bool        -- ^ blamed by update count, the value having come back
  , esRetry  :: !Int         -- ^ of 'esFails', those that failed inside retry, trying to block
  } deriving (Eq, Show)

instance Semigroup EdgeStat where
  EdgeStat a b c d e f g h i j <> EdgeStat a' b' c' d' e' f' g' h' i' j' =
    EdgeStat (a + a') (b + b') (c .|. c') (d + d') (e + e') (f + f') (g + g') (S.union h h') (i || i') (j + j')

instance Monoid EdgeStat where
  mempty = EdgeStat 0 0 0 0 0 0 0 S.empty False 0

-- | Calls at or above p99, decomposed. Latency is estimated from bucket
-- midpoints (within 1/16); the rest is exact.
data Tail = Tail
  { tP50, tP99, tP999 :: !Word64
  , tCount, tRetried, tGC :: !Int
  , tLatency :: !Double
  , tWasted, tBlocked :: !Word64
  } deriving (Eq, Show)

data SiteInfo = SiteInfo
  { siFrames :: [Info]
  , siStats  :: Stats
  , siTail   :: Maybe Tail
  } deriving (Eq, Show)

data Profile = Profile
  { pSites     :: IntMap SiteInfo
  , pProv      :: Map Info Prov
  , pEdges     :: Map Edge EdgeStat
  , pWrites    :: IntMap Word64           -- ^ commits that wrote each watched TVar
  , pWatches   :: IntMap (WatchReason, Info)
  , pEscalate  :: [Msg]
  , pSummary   :: Maybe Summary
  , pDuration  :: Timestamp
  , pBad       :: [String]
  , pTruncated :: Maybe String
  , pRts       :: Maybe T.Text
  , pFacts     :: Facts                   -- ^ from the build, if it could be asked
  }

perSecond :: Profile -> Double -> Double
perSecond p n = n * 1e9 / fromIntegral (max 1 (pDuration p))

writeRate :: Profile -> Watch -> Double
writeRate p w = perSecond p (fromIntegral (IM.findWithDefault 0 w (pWrites p)))

-- | Mean length of a site's failed attempts, in ns.
meanAttempt :: SiteInfo -> Double
meanAttempt si = fromIntegral (sWasted st) / fromIntegral (max 1 (sFails st))
  where st = siStats si

analyze :: Log -> Profile
analyze Log {..} = Profile
  { pSites     = IM.mapWithKey site (IM.unionWith (<>) (mempty <$ frames) stats)
  , pProv      = prov
  , pEdges     = M.fromListWith (<>) [edge g | MEdge g <- msgs, gOutcome g /= Vetoed]
  , pWrites    = IM.fromListWith (+) [(w, n) | MWrites w n <- msgs]
  , pWatches   = IM.fromList [(w, (r, i)) | MWatch w r i <- msgs]
  , pEscalate  = [m | m <- msgs, isEscalation m]
  , pSummary   = L.find (const True) [z | MSummary z <- msgs]
  , pDuration  = lEnd
  , pBad       = lBad
  , pTruncated = lTruncated
  , pRts       = lRts
  , pFacts     = noFacts
  }
  where
    prov = M.fromList [(i, p) | MInfo i p <- lMsgs]
    -- Sites the user cannot tell apart are one site. Identity is the
    -- provenance of the frames that have a source location: GHC may clone a
    -- body (specialising `map f [a, b]`), and RTS or library continuations
    -- have no location to show.
    canon = IM.fromList [(s, minimum ss) | ss <- M.elems groups, s <- ss]
      where
        groups = M.fromListWith (++) [(key s fs, [s]) | MSite s fs <- lMsgs]
        key s fs = case [(pLabel p, pFile p, pSpan p) | i <- fs, Just p <- [M.lookup i prov], not (T.null (pFile p))] of
          [] -> Left s
          k  -> Right k
    re s = IM.findWithDefault s s canon
    msgs = map relabel lMsgs
    relabel = \case
      MSite s fs        -> MSite (re s) fs
      MStats s st       -> MStats (re s) st
      MEdge g           -> MEdge g { gVictim = re (gVictim g), gWriter = re (gWriter g) }
      MEscalate t s l n -> MEscalate t (re s) l n
      m                 -> m

    frames = IM.fromList [(s, fs) | MSite s fs <- msgs]
    stats  = IM.fromListWith (<>) [(s, st) | MStats s st <- msgs]

    edge EdgeMsg {..} =
      ( Edge gVictim gWatch gWriter
      , EdgeStat
          { esFails = gFails, esWasted = fromIntegral gWasted, esMask = gMask
          , esFields = gFields, esCon = gCon, esOpaque = gOpaque, esLocked = gLocked
          , esValues = if gOld == 0 then S.empty else S.singleton gOld
          , esAba = gOutcome == Unexplained && gWatch /= 0
          , esRetry = if gOutcome == RetryInvalid then gFails else 0 } )

    site s st = SiteInfo (IM.findWithDefault [] s frames) st (tailOf st)

    tailOf st
      | sCommits st == 0 = Nothing
      | otherwise = Just Tail
          { tP50 = quantile 0.5 calls, tP99 = p99, tP999 = quantile 0.999 calls
          , tCount   = sum (bCalls <$> top)
          , tRetried = sum (bRetried <$> top)
          , tGC      = sum (bGC <$> top)
          , tLatency = sum (IM.mapWithKey (\b k -> bucketMid b * fromIntegral (bCalls k)) top)
          , tWasted  = sum (bWasted <$> top)
          , tBlocked = sum (bBlocked <$> top)
          }
      where
        calls = bCalls <$> sHist st
        p99 = quantile 0.99 calls
        top = IM.filterWithKey (\b _ -> bucketLow b >= p99) (sHist st)

    isEscalation = \case MEscalate {} -> True; MRelease {} -> True; _ -> False
