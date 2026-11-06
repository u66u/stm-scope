-- | The messages the runtime shim writes into the eventlog, and their decoder.
--
-- The byte layout is specified once, at the top of @rt/stmscope.c@; this
-- module is its exact mirror. Every message is a 'UserBinaryMessage' whose
-- payload starts with @'S'@ and a tag byte. Anything else in the eventlog
-- (including the program's own binary events) is ignored.
--
-- The shim aggregates before it writes: stats and edges arrive as periodic
-- per-capability deltas, which is why both are monoids here.
module StmScope.Wire
  ( -- * Identifiers
    Site, Watch, Tid, Info
    -- * Messages
  , Msg (..), Outcome (..), EdgeMsg (..), Prov (..), pArity, Stats (..), Bucket (..)
  , Summary (..), WatchReason (..), Release (..)
    -- * Latency histogram
  , bucket, bucketLow, bucketMid, quantile
    -- * Reading an eventlog
  , Log (..), readLog, decode
  ) where

import Control.Monad (replicateM, unless)
import Data.Binary.Get (Get, getLazyByteStringNul, getWord16le, getWord32le, getWord64le, getWord8, runGetOrFail)
import Data.Bits (countLeadingZeros, shiftL, shiftR, (.&.))
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word
import GHC.RTS.Events (Event (..), EventInfo (RtsIdentifier, UserBinaryMessage), Timestamp)
import GHC.RTS.Events.Incremental (readEvents, readHeader)

type Site  = Int     -- ^ 0 is "unknown"
type Watch = Int     -- ^ id of a TVar the shim watches; 0 is "unknown"
type Tid   = Word64  -- ^ Haskell thread id
type Info  = Word64  -- ^ raw info pointer; resolved through 'MInfo'

data Outcome = Commit | Conflict | InFlight | Locked | Unexplained | RetryInvalid | Vetoed | Exception
  deriving (Eq, Ord, Show, Enum, Bounded)

data WatchReason = ForCulprit | ForReadSet
  deriving (Eq, Ord, Show, Enum, Bounded)

data Release = OnCommit | OnBlock | OnAbort | OnWatchdog | OnDeath
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Failed attempts of one victim site, blamed on one TVar and one writer site.
data EdgeMsg = EdgeMsg
  { gVictim :: !Site, gWatch :: !Watch, gWriter :: !Site, gOutcome :: !Outcome
  , gFails :: !Int
  , gFields, gCon, gOpaque, gLocked :: !Int  -- ^ how the TVar was found: same constructor,
                                             --   another one, not inspectable, locked
  , gWasted :: !Word64                       -- ^ ns, split evenly over an attempt's culprits
  , gMask :: !Word64                         -- ^ payload words seen changed
  , gOld :: !Info }                          -- ^ a sample of the TVar's old value
  deriving (Eq, Show)

-- | Info-table provenance. Every field may be empty. Word counts and the
-- constructor name are set for constructors only.
data Prov = Prov
  { pPtrs, pNptrs :: !Int, pCon, pTable, pType, pLabel, pModule, pFile, pSpan :: !Text }
  deriving (Eq, Show)

-- | Payload words of a constructor.
pArity :: Prov -> Int
pArity p = pPtrs p + pNptrs p

-- | Calls whose commit latency fell in one histogram bucket.
data Bucket = Bucket { bCalls, bRetried, bGC :: !Int, bWasted, bBlocked :: !Word64 }
  deriving (Eq, Show)

instance Semigroup Bucket where
  Bucket a b c d e <> Bucket a' b' c' d' e' = Bucket (a + a') (b + b') (c + c') (d + d') (e + e')

data Stats = Stats
  { sCommits, sFails, sWaits, sWakes, sSpurious, sAborts, sVetoed, sConflicts :: !Int
  , sLatency, sBlocked, sWasted :: !Word64   -- ^ ns sums
  , sHist :: !(IntMap Bucket) }              -- ^ by commit latency 'bucket'
  deriving (Eq, Show)

instance Semigroup Stats where
  Stats a b c d e f g h i j k l <> Stats a' b' c' d' e' f' g' h' i' j' k' l' =
    Stats (a + a') (b + b') (c + c') (d + d') (e + e') (f + f') (g + g') (h + h')
          (i + i') (j + j') (k + k') (IM.unionWith (<>) l l')

instance Monoid Stats where
  mempty = Stats 0 0 0 0 0 0 0 0 0 0 0 IM.empty

data Summary = Summary
  { zVersion, zDepth, zEscalate :: !Int
  , zDroppedTx, zDroppedSites, zDroppedWatch, zDroppedEdges, zCanaryChecked, zCanaryBad :: !Word64 }
  deriving (Eq, Show)

data Msg
  = MSite !Site ![Info]                  -- ^ frame 0 is the transaction body
  | MInfo !Info !Prov
  | MWatch !Watch !WatchReason !Info     -- ^ watching starts; value's info
  | MStats !Site !Stats
  | MEdge !EdgeMsg
  | MWrites !Watch !Word64               -- ^ commits that wrote a watched TVar
  | MEscalate !Tid !Site !Int !Int       -- ^ level, protected TVars
  | MRelease !Tid !Release
  | MSummary !Summary
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Histogram: log-linear, exact below 16 ns, then 8 buckets per octave.
-- 'bucket' must agree with bucket() in stmscope.c bit for bit.

bucket :: Word64 -> Int
bucket ns
  | ns < 16   = fromIntegral ns
  | otherwise = (e - 2) * 8 + fromIntegral ((ns `shiftR` (e - 3)) .&. 7)
  where e = 63 - countLeadingZeros ns

-- | Smallest value that falls in a bucket.
bucketLow :: Int -> Word64
bucketLow b
  | b < 16    = fromIntegral b
  | otherwise = fromIntegral (8 + r) `shiftL` (q - 1)   -- q - 1 = e - 3
  where (q, r) = b `divMod` 8

-- | Midpoint of a bucket: within 1/16 of any value in it.
bucketMid :: Int -> Double
bucketMid b
  | b < 16    = fromIntegral b
  | otherwise = fromIntegral (bucketLow b) * (1 + 1 / (2 * fromIntegral (8 + b `mod` 8)))

-- | @quantile q h@: lower bound of the bucket holding the q-th fraction.
quantile :: Double -> IntMap Int -> Word64
quantile q h = go 0 (IM.toAscList h)
  where
    rank = ceiling (q * fromIntegral (sum h)) `max` 1 :: Int
    go _ [] = 0
    go acc ((b, n) : rest)
      | acc + n >= rank = bucketLow b
      | otherwise       = go (acc + n) rest

------------------------------------------------------------------------------
-- Decoding

wireVersion :: Int
wireVersion = 4

-- | Decode one payload. 'Nothing' for payloads that aren't ours; 'Left' for
-- ours that are malformed (a shim/CLI version mismatch, most likely).
decode :: B.ByteString -> Maybe (Either String Msg)
decode p = case B.uncons p of
  Just (0x53, rest) -> Just $ case runGetOrFail msg (BL.fromStrict rest) of
    Right (left, _, m) | BL.null left -> Right m
                       | otherwise    -> Left "trailing bytes"
    Left (_, off, e)                  -> Left (e <> " at byte " <> show (off + 1))
  _ -> Nothing

msg :: Get Msg
msg = getWord8 >>= \case
  0x44 {- D -} -> MSite <$> u32 <*> (u8 >>= \n -> replicateM n u64)
  0x49 {- I -} -> MInfo <$> u64 <*> (Prov <$> u16 <*> u16 <*> str <*> str <*> str <*> str <*> str <*> str <*> str)
  0x56 {- V -} -> MWatch <$> u32 <*> enum <*> u64
  0x53 {- S -} -> do
    site <- u32
    [a, b, c, d, e, f, g, h] <- replicateM 8 u32
    [i, j, k] <- replicateM 3 u64
    hist <- u16 >>= \n -> replicateM n ((,) <$> u16 <*> (Bucket <$> u32 <*> u32 <*> u32 <*> u64 <*> u64))
    pure (MStats site (Stats a b c d e f g h i j k (IM.fromListWith (<>) hist)))
  0x47 {- G -} -> fmap MEdge $
    EdgeMsg <$> u32 <*> u32 <*> u32 <*> enum <*> u32 <*> u32 <*> u32 <*> u32 <*> u32 <*> u64 <*> u64 <*> u64
  0x4e {- N -} -> MWrites <$> u32 <*> u64
  0x45 {- E -} -> MEscalate <$> u64 <*> u32 <*> u8 <*> u32
  0x52 {- R -} -> MRelease <$> u64 <*> enum
  0x5a {- Z -} -> do
    zVersion <- u32
    unless (zVersion == wireVersion) $
      fail ("wire version " <> show zVersion <> ", expected " <> show wireVersion)
    zDepth <- u32; zEscalate <- u32
    [zDroppedTx, zDroppedSites, zDroppedWatch, zDroppedEdges, zCanaryChecked, zCanaryBad] <- replicateM 6 u64
    pure (MSummary Summary {..})
  t -> fail ("unknown tag " <> show t)

u8, u16, u32 :: Num a => Get a
u8  = fromIntegral <$> getWord8
u16 = fromIntegral <$> getWord16le
u32 = fromIntegral <$> getWord32le

u64 :: Get Word64
u64 = getWord64le

str :: Get Text
str = decodeUtf8Lenient . BL.toStrict <$> getLazyByteStringNul

enum :: forall a. (Enum a, Bounded a) => Get a
enum = u8 >>= \n ->
  if n <= fromEnum (maxBound :: a) then pure (toEnum n) else fail ("bad enum " <> show n)

------------------------------------------------------------------------------
-- Eventlog

data Log = Log
  { lMsgs :: [Msg]
  , lEnd  :: !Timestamp                   -- ^ last event seen: the run's length
  , lBad  :: [String]                     -- ^ malformed messages of ours
  , lTruncated :: Maybe String            -- ^ the eventlog itself ended badly
  , lRts :: Maybe Text                    -- ^ the RTS that wrote it ("GHC-9.6.7 rts_thr")
  }

data Acc = Acc ![Msg] !Timestamp ![String] !(Maybe Text)

-- | One streaming pass; only our messages are kept, and there are few of
-- them (the shim aggregates), so memory is bounded by the program's shape,
-- not by how long it ran.
readLog :: FilePath -> IO (Either String Log)
readLog path = do
  bytes <- BL.readFile path
  pure $ do
    (h, body) <- readHeader bytes
    let (evs, trunc) = readEvents h body
        Acc ms end bad rts = L.foldl' step (Acc [] 0 [] Nothing) evs
    pure (Log (reverse ms) end (reverse bad) trunc rts)
  where
    step (Acc ms end bad rts) ev = case evSpec ev of
      UserBinaryMessage p -> case decode p of
        Just (Right m) -> Acc (m : ms) end' bad rts
        Just (Left e)  -> Acc ms end' (e : bad) rts
        Nothing        -> Acc ms end' bad rts
      RtsIdentifier _ i -> Acc ms end' bad (Just i)
      _ -> Acc ms end' bad rts
      where end' = max (evTime ev) end
