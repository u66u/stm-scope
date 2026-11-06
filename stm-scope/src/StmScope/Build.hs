-- | Building an instrumented executable and running it.
--
-- Nothing in the user's project changes. The shim is compiled by the user's
-- own GHC (so it sees that RTS's headers and struct layouts), merged into one
-- relocatable object, and handed to cabal on the command line. cabal applies
-- command-line --ghc-options/--ld-options to local packages only, so
-- dependencies come from the store unchanged; a separate --builddir keeps the
-- user's dist-newstyle untouched.
module StmScope.Build
  ( Ghc (..), findGhc, shimObject, factsHelper, ghcFlags, linkFlags, wrapped, builddir
  , cabalBuild, RunOpts (..), profileRun
  ) where

import Control.Monad (unless, when)
import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), die)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>))
import System.Process

import Paths_stm_scope (getDataFileName)

data Ghc = Ghc { ghcPath :: FilePath, ghcVersion :: String, ghcInfo :: [(String, String)] }

-- | The RTS entry points the shim interposes on (see rt/stmscope.c).
wrapped :: [String]
wrapped =
  [ "stg_atomicallyzh", "stmStartTransaction", "stmCommitTransaction", "stmAbortTransaction"
  , "stmWait", "stmReWait", "gcStableNameTable", "exitScheduler" ]

builddir :: FilePath
builddir = "dist-stm-scope"

findGhc :: FilePath -> IO Ghc
findGhc path = do
  info <- read <$> readProcess path ["--info"] ""
  let get k = fromMaybe "" (lookup k info)
  unless (get "target os" == "OSLinux") $
    die ("stm-scope: " <> path <> " targets " <> get "target os" <> "; only Linux is supported (ld --wrap)")
  unless ("thr" `elem` words (get "RTS ways")) $
    die ("stm-scope: " <> path <> " has no threaded RTS")
  pure (Ghc path (get "Project version") info)

-- | Compile (once per GHC version and shim source) and return the object.
shimObject :: Ghc -> IO FilePath
shimObject g = do
  srcs@[c, cmm] <- traverse getDataFileName ["rt/stmscope.c", "rt/hook.cmm"]
  cached g srcs "stmscope.o" $ \dir out -> do
    let ghc args = callProcess (ghcPath g) ("-c" : args)
        info k d = maybe d words (lookup k (ghcInfo g))
    ghc ["-optc-DTHREADED_RTS", "-optc-O2", c, "-o", dir </> "c.o"]
    ghc [cmm, "-o", dir </> "cmm.o"]
    case info "Merge objects command" ["ld"] of
      ld : pre -> callProcess ld (pre <> info "Merge objects flags" ["-r"] <> [dir </> "c.o", dir </> "cmm.o", "-o", out])
      []       -> die "stm-scope: ghc reports no object merge command"

-- | The facts helper reads .hi and .hie files, whose formats belong to one
-- GHC: it is compiled by that GHC, from boot packages only.
factsHelper :: Ghc -> IO FilePath
factsHelper g = do
  src <- getDataFileName "facts/Facts.hs"
  cached g [src] "stm-scope-facts" $ \dir out -> do
    hPutStrLn stderr ("stm-scope: compiling the facts helper with GHC " <> ghcVersion g <> " (once)")
    callProcess (ghcPath g) ["-v0", "-O", "-package", "ghc", "-outputdir", dir </> "obj", src, "-o", out]

-- | Build a file once per GHC version and source contents, in the XDG cache.
cached :: Ghc -> [FilePath] -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO FilePath
cached g srcs name make = do
  key <- fnv1a . B.concat <$> traverse B.readFile srcs
  dir <- getXdgDirectory XdgCache ("stm-scope" </> ghcVersion g </> showHex key "")
  let out = dir </> name
  have <- doesFileExist out
  unless have $ do
    createDirectoryIfMissing True dir
    make dir (out <> ".tmp")
    renameFile (out <> ".tmp") out   -- atomic: concurrent builds never see half a file
  pure out
  where
    fnv1a = B.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001b3) (0xcbf29ce484222325 :: Word64)

-- | Compile flags for local packages. -threaded because the shim is built
-- against the threaded RTS; IPE tables name sites and values; .hie files
-- (next to the .hi files) are what the facts helper reads.
ghcFlags :: [String]
ghcFlags = ["-threaded", "-rtsopts", "-finfo-table-map", "-fdistinct-constructor-tables", "-fwrite-ide-info"]

linkFlags :: FilePath -> [String]
linkFlags obj = [obj, "-Wl," <> L.intercalate "," (map ("--wrap=" <>) wrapped)]

cabalBuild :: Ghc -> FilePath -> [String] -> IO ExitCode
cabalBuild g obj args = do
  -- shortcut: cabal splits these on spaces, so the cache path must not contain any.
  when (' ' `elem` obj) $ die ("stm-scope: cache path has a space: " <> obj)
  let flags = [ "build", "--builddir=" <> builddir, "--with-compiler=" <> ghcPath g
              , "--ghc-options=" <> unwords ghcFlags, "--ld-options=" <> unwords (linkFlags obj) ]
  (_, _, _, h) <- createProcess (proc "cabal" (flags <> args)) { delegate_ctlc = True }
  waitForProcess h

data RunOpts = RunOpts
  { roLog      :: FilePath
  , roDepth    :: Maybe Int
  , roEscalate :: Maybe Int
  , roCanary   :: Bool
  }

-- | Run an instrumented program with the eventlog going to 'roLog'.
profileRun :: RunOpts -> FilePath -> [String] -> IO ExitCode
profileRun RunOpts {..} exe args = do
  env <- getEnvironment
  -- shortcut: GHCRTS is split on whitespace, so the log path must not contain any.
  when (any (`elem` (" \t" :: String)) roLog) $ die "stm-scope: eventlog path must not contain spaces"
  -- -l-s: everything but scheduler events, which can outweigh ours 1000:1
  let rts  = "-l-s -ol" <> roLog
      ours = [("GHCRTS", maybe rts (<> " " <> rts) (lookup "GHCRTS" env))]
          <> [("STMSCOPE_DEPTH", show d) | Just d <- [roDepth]]
          <> [("STMSCOPE_ESCALATE", show k) | Just k <- [roEscalate]]
          <> [("STMSCOPE_CANARY", "1") | roCanary]
      env' = ours <> filter ((`notElem` map fst ours) . fst) env
  (_, _, _, h) <- createProcess (proc exe args) { env = Just env', delegate_ctlc = True }
  waitForProcess h
