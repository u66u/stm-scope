-- | Static facts from the build (field names, which fields a site reads,
-- TVar names), asked of the stm-scope-facts helper that `stm-scope build`
-- leaves in the build directory. The helper is compiled by the project's
-- GHC; see facts/Facts.hs for the protocol and for what "reads" means.
--
-- Facts are advisory. A layout is used only if its word counts match the
-- running program's, so a stale build directory degrades the report to
-- payload word numbers rather than to wrong field names.
module StmScope.Facts
  ( Facts (..), Layout (..), Reads (..), noFacts, askFacts, parseFacts
  , layoutOf, readsOf, tvarNames, fieldsOf
  ) where

import Control.Exception (IOException, try)
import Data.Bits (testBit)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import StmScope.Wire

data Layout = Layout { lTyCon :: !Text, lPtrs, lNptrs :: !Int, lWords :: [Text] }
  deriving (Eq, Show)

-- | What a site may read of a record type.
data Reads = Fields (Set Text) | Whole
  deriving (Eq, Show)

data Facts = Facts
  { fComp  :: IntMap Text                      -- ^ site: its build component
  , fCons  :: Map (Text, Text) Layout          -- ^ (component, constructor)
  , fReads :: Map (Site, Text) Reads           -- ^ (site, type)
  , fTVars :: Map (Site, Text) [(Text, Text)]  -- ^ (site, contents type): name, location
  } deriving (Eq, Show)

noFacts :: Facts
noFacts = Facts IM.empty M.empty M.empty M.empty

-- | Ask the helper in a build directory about these sites. Nothing if there
-- is no helper there (not built with `stm-scope build`) or it failed.
askFacts :: FilePath -> [(Site, Prov)] -> IO (Either String Facts)
askFacts dir sites = do
  let helper = dir </> "stm-scope-facts"
  have <- doesFileExist helper
  if not have then pure (Left ("no " <> helper)) else do
    let q = unlines [ L.intercalate "\t" ["site", show s, T.unpack (pModule v), T.unpack (pFile v), T.unpack (pSpan v)]
                    | (s, v) <- sites, not (T.null (pFile v)) ]
    r <- try (readProcessWithExitCode helper [dir] q)
    pure $ case r of
      Left (e :: IOException)            -> Left (show e)
      Right (ExitSuccess, out, _)        -> Right (parseFacts (T.pack out))
      Right (ExitFailure c, _, err)      -> Left (helper <> " exited " <> show c <> ": " <> take 300 err)

parseFacts :: Text -> Facts
parseFacts = L.foldl' line noFacts . map (T.splitOn "\t") . T.lines
  where
    line f = \case
      ["comp", s, c] | Just i <- num s -> f { fComp = IM.insert i c (fComp f) }
      ["con", c, con, tc, p, n, ws] | Just p' <- num p, Just n' <- num n ->
        f { fCons = M.insert (c, con) (Layout tc p' n' (T.splitOn "," ws)) (fCons f) }
      ["read", s, tc, fs] | Just i <- num s ->
        f { fReads = M.insert (i, tc) (if fs == "*" then Whole else Fields (S.fromList (filter (not . T.null) (T.splitOn "," fs)))) (fReads f) }
      ["tvar", s, tc, n, l] | Just i <- num s -> f { fTVars = M.insertWith (<>) (i, tc) [(n, l)] (fTVars f) }
      _ -> f
    num t = case reads (T.unpack t) of [(n, "")] -> Just n; _ -> Nothing

-- | The layout of a value's constructor as seen from a site, if the build's
-- facts agree with the running program about its shape.
layoutOf :: Facts -> Site -> Prov -> Maybe Layout
layoutOf f s v = do
  c <- IM.lookup s (fComp f)
  l <- M.lookup (c, pCon v) (fCons f)
  if lPtrs l == pPtrs v && lNptrs l == pNptrs v && length (lWords l) == pArity v then Just l else Nothing

-- | What a site reads of a type: Nothing if the facts do not cover the site.
readsOf :: Facts -> Site -> Text -> Maybe Reads
readsOf f s tc
  | IM.member s (fComp f) = Just (M.findWithDefault (Fields S.empty) (s, tc) (fReads f))
  | otherwise = Nothing

-- | TVars, holding a type that passes the test, that all these sites mention.
tvarNames :: Facts -> [Site] -> (Text -> Bool) -> [(Text, Text)]
tvarNames f ss holds = case [S.fromList (concat [ns | ((s', tc), ns) <- M.toList (fTVars f), s' == s, holds tc]) | s <- ss, IM.member s (fComp f)] of
  [] -> []
  xs -> S.toList (foldr1 S.intersection xs)

-- | Source fields a payload word mask covers.
fieldsOf :: Layout -> Word64 -> Set Text
fieldsOf l m = S.fromList [w | (i, w) <- zip [0 ..] (lWords l), i < 64, testBit m i]
