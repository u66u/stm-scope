-- | Turning a 'Profile' into ranked findings. Rules follow PLAN.md section 7.2;
-- the first rule that matches a (victim site, TVar) pair wins, and findings
-- are ranked by the time they would save, not by how often they occur.
module StmScope.Verdict
  ( Verdict (..), Finding (..), Writer (..), findings, fullMask
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Word (Word64)

import StmScope.Analyze
import StmScope.Facts
import StmScope.Wire

data Verdict
  = Split          -- ^ writers change a strict subset of a record's fields
  | HotScalar      -- ^ a one-word value written from many places or very often
  | LongVictim     -- ^ the victim runs longer than the writers' inter-arrival time
  | TrueConflict   -- ^ the same data really is contended
  | AbaChurn       -- ^ a read set was invalidated and restored in between
  | LockCollision  -- ^ failures caused by a concurrent commit's lock, not a value
  | Herd           -- ^ many wake-ups that find nothing to do
  | ByDesign       -- ^ blocked in 'retry', barely contended: not a problem
  | Unattributed   -- ^ cost with no recorded writer
  deriving (Eq, Ord, Show, Enum, Bounded)

data Writer = Writer { wSite :: !Site, wStat :: !EdgeStat }
  deriving (Eq, Show)

data Finding = Finding
  { fdVerdict :: !Verdict
  , fdVictim  :: !Site
  , fdTVar    :: !Watch          -- ^ 0 for site-level findings
  , fdTVars   :: !Int            -- ^ TVars involved (more than one for a spread 'LongVictim')
  , fdValue   :: Maybe Prov      -- ^ constructor of the TVar's contents
  , fdWriters :: [Writer]        -- ^ by damage, worst first; site 0 = not yet watched
  , fdStat    :: !EdgeStat       -- ^ all writers together
  , fdSaving  :: !Double         -- ^ ns of discarded work this would remove
  , fdLayout  :: Maybe Layout    -- ^ field of each payload word, from the build
  , fdReads   :: Maybe Reads     -- ^ what the victim may read of the value, from the build
  } deriving (Eq, Show)

-- Thresholds, named so the report can quote them.
hotWriters :: Int
hotWriters = 3

hotRate, lockShare :: Double
hotRate   = 10_000
lockShare = 0.5

fullMask :: Int -> Word64
fullMask n = if n >= 64 then maxBound else (1 `shiftL` n) - 1

findings :: Profile -> [Finding]
findings p = L.sortOn (Down . fdSaving) . foldHerds $
  concatMap victim (M.toList byVictim) <> mapMaybe siteFinding (IM.toList (pSites p))
  where
    byVictim = M.fromListWith (M.unionWith (<>))
      [(eVictim e, M.singleton (eTVar e) [Writer (eAttacker e) s]) | (e, s) <- M.toList (pEdges p)]

    -- A victim that loses to many TVars, none dominant, has one problem (its
    -- read set is too big for its duration), not hundreds of small ones.
    victim (v, byTVar)
      | M.size named >= 4, maximum costs < 0.5 * sum costs =
          spread : map (pair v) (M.toList unnamed)
      | otherwise = map (pair v) (M.toList byTVar)
      where
        (unnamed, named) = M.partitionWithKey (\w _ -> w == 0) byTVar
        costs   = map (esWasted . foldMap wStat) (M.elems named)
        merged  = M.fromListWith (<>) [(wSite x, wStat x) | ws <- M.elems named, x <- ws]
        writers = byDamage [Writer s st | (s, st) <- M.toList merged]
        total   = foldMap wStat writers
        verdict = if retrying total then Herd else LongVictim
        spread  = Finding verdict v 0 (M.size named) Nothing writers total (esWasted total) Nothing Nothing

    pair v (w, ws0) = Finding verdict v w 1 value ws total (esWasted total) layout victimReads
      where
        ws    = byDamage ws0
        total = foldMap wStat ws
        value = listToMaybe (mapMaybe (`M.lookup` pProv p) (S.toList (esValues total)))
        arity = maybe (-1) pArity value
        layout = value >>= layoutOf (pFacts p) v
        victimReads = layout >>= readsOf (pFacts p) v . lTyCon
        changed = foldr ((.|.) . esMask . wStat) 0 known
        -- with facts: the victim must not read what the writers change
        unread = case (layout, victimReads) of
          (Just l, Just (Fields r)) -> S.disjoint r (fieldsOf l changed)
          (Just _, Just Whole)      -> False
          _                         -> True   -- no facts: the report says what to check
        known = filter ((/= 0) . wSite) ws
        culprits = esFields total + esCon total + esOpaque total + esLocked total
        verdict
          | retrying total                                            = Herd
          | w == 0                                                    = Unattributed
          | esAba total                                               = AbaChurn
          | fromIntegral (esLocked total) > lockShare * fromIntegral (max 1 culprits) = LockCollision
          | arity >= 2, not (null known), all (partial . wStat) known, disjoint, unread = Split
          | arity `elem` [0, 1], length known > hotWriters || writeRate p w > hotRate = HotScalar
          | not (M.member (w, v) masks), outlasts v (writeRate p w)   = LongVictim  -- a reader
          | otherwise                                                 = TrueConflict
        -- the victim's own writes to w (seen when it was the writer) must
        -- not touch what the writers change, or the split cannot help
        disjoint = changed .&. M.findWithDefault 0 (w, v) masks == 0
        partial s = esCon s == 0 && esOpaque s == 0 && esFields s > 0
                 && esMask s /= 0 && esMask s .&. fullMask arity /= fullMask arity

    -- Failures inside retry are waiters losing a race to park: herd cost.
    retrying s = 2 * esRetry s > esFails s

    -- Conflicting writes arrive at rate r; an attempt of length d survives
    -- with probability exp(-r d) (Poisson). r d > 1: most attempts cannot finish.
    outlasts v r = maybe False (\si -> meanAttempt si / 1e9 * r > 1) (IM.lookup v (pSites p))

    -- Woken waiters that find nothing to do run once more and block again:
    -- each extra wait is a re-run (not in the wasted total, so an estimate).
    siteFinding (s, si)
      | sWaits st >= 100, sWaits st >= 4 * max 1 (sCommits st) =
          Just (site Herd (fromIntegral (sLatency st) * waits / (waits + fromIntegral (sCommits st))))
      | sWaits st > 0, sBlocked st > sLatency st, sConflicts st * 10 < max 1 (sCommits st) =
          Just (site ByDesign 0)
      | otherwise = Nothing
      where
        st = siStats si
        waits = fromIntegral (sWaits st) :: Double
        site v saving = Finding v s 0 0 Nothing [] mempty saving Nothing Nothing

    -- One herd per victim: its failed parks and its re-runs are one problem.
    foldHerds fs = M.elems (M.fromListWith join [(fdVictim f, f) | f <- herds]) <> rest
      where
        (herds, rest) = L.partition ((== Herd) . fdVerdict) fs
        join a b = a { fdTVar = 0, fdTVars = fdTVars a + fdTVars b, fdWriters = fdWriters a <> fdWriters b
                     , fdStat = fdStat a <> fdStat b, fdSaving = fdSaving a + fdSaving b }

    masks = M.fromListWith (.|.) [((eTVar e, eAttacker e), esMask s) | (e, s) <- M.toList (pEdges p)]
    byDamage = L.sortOn (Down . esWasted . wStat)
