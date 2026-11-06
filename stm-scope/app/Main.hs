module Main (main) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (join, when)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import System.Exit (ExitCode (..), die, exitWith)
import System.Directory (createFileLink, removeFile)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

import StmScope.Analyze (Profile (..), SiteInfo (..), analyze)
import StmScope.Facts (askFacts, noFacts)
import StmScope.Build
import StmScope.Json (json)
import StmScope.Report (diff, render)
import StmScope.Wire (lMsgs, readLog)

main :: IO ()
main = join . customExecParser (prefs showHelpOnEmpty) $ info (commands <**> helper) $
  fullDesc <> progDesc "STM contention profiler for GHC: find the TVar layout change worth making"
  where
    commands = hsubparser $ mconcat
      [ command "build" $ info buildCmd $ progDesc
          "cabal build with the shim linked in (separate builddir; your project is untouched)"
      , command "run" $ info runCmd $ progDesc "run an instrumented program, then report"
      , command "report" $ info reportCmd $ progDesc "analyse an eventlog written by an instrumented program"
      , command "diff" $ info diffCmd $ progDesc "compare two eventlogs: what a change did to each site and finding"
      , command "link-flags" $ info linkCmd $ progDesc
          "print GHC flags that instrument a link (for stack, Nix, plain ghc)"
      ]

withGhc :: Parser FilePath
withGhc = strOption (long "with-compiler" <> short 'w' <> metavar "GHC" <> value "ghc" <> showDefault
  <> help "the compiler your project builds with; the shim is compiled by it")

topN :: Parser Int
topN = option auto (long "top" <> short 'n' <> metavar "N" <> value 10 <> showDefault <> help "findings to show")

buildCmd :: Parser (IO ())
buildCmd = go <$> withGhc <*> many (strArgument (metavar "-- CABAL-ARGS..."))
  where
    go w args = do
      g <- findGhc w
      obj <- shimObject g
      code <- cabalBuild g obj args
      when (code == ExitSuccess) $ do
        try (factsHelper g) >>= \case
          Right exe -> do
            let link = builddir </> "stm-scope-facts"
            _ <- try @IOException (removeFile link)
            createFileLink exe link
          Left (e :: SomeException) -> hPutStrLn stderr ("stm-scope: no facts helper (" <> show e <> "); reports will show payload words, not field names")
        hPutStrLn stderr $ "stm-scope: built in " <> builddir <> "; now: stm-scope run -- $(cabal list-bin --builddir="
          <> builddir <> " EXE)"
      exitWith code

linkCmd :: Parser (IO ())
linkCmd = go <$> withGhc
  where go w = findGhc w >>= shimObject >>= \o -> putStrLn (unwords (ghcFlags <> map ("-optl" <>) (linkFlags o)))

runCmd :: Parser (IO ())
runCmd = go <$> out <*> opts <*> format <*> buildDir <*> strArgument (metavar "EXE") <*> many (strArgument (metavar "ARGS..."))
  where
    out  = optional (strOption (long "eventlog" <> short 'o' <> metavar "FILE" <> help "default: EXE.stm.eventlog"))
    opts = RunOpts ""
      <$> optional (option auto (long "depth" <> metavar "D" <> help "caller frames per site (0-6, default 3)"))
      <*> optional (option auto (long "escalate" <> metavar "K"
            <> help "experimental: after K conflicts, let a starving transaction through by vetoing rivals"))
      <*> switch (long "canary" <> help "verify on every commit that TRec contents survive (slow)")
    go file o fmt dir exe args = do
      let o' = o { roLog = fromMaybe (takeFileName exe <> ".stm.eventlog") file }
      code <- profileRun o' exe args
      report fmt dir (roLog o')
      exitWith code

reportCmd :: Parser (IO ())
reportCmd = report <$> format <*> buildDir <*> strArgument (metavar "EVENTLOG")

diffCmd :: Parser (IO ())
diffCmd = go <$> buildDir <*> strArgument (metavar "A.eventlog") <*> strArgument (metavar "B.eventlog")
  where go dir a b = diff <$> load dir a <*> load dir b >>= T.putStr

-- | Text with the top N findings, or JSON with all of them.
format :: Parser (Profile -> T.Text)
format = flag' json (long "json" <> help "machine-readable report, every finding") <|> render <$> topN

buildDir :: Parser FilePath
buildDir = strOption (long "builddir" <> metavar "DIR" <> value builddir <> showDefault
  <> help "where `stm-scope build` left the facts helper (field and TVar names)")

report :: (Profile -> T.Text) -> FilePath -> FilePath -> IO ()
report out dir path = load dir path >>= T.putStr . out

-- | An eventlog's profile, with the build's facts if they can be had.
load :: FilePath -> FilePath -> IO Profile
load dir path = readLog path >>= \case
  Left e -> die ("stm-scope: " <> path <> ": " <> e)
  Right l -> do
    when (null (lMsgs l)) $ die $
      "stm-scope: " <> path <> " has no stm-scope data: was the program built with `stm-scope build`, and did it run a transaction?"
    let p = analyze l
        bodies = [(s, v) | (s, si) <- IM.toList (pSites p), i : _ <- [siFrames si], Just v <- [M.lookup i (pProv p)]]
    facts <- askFacts dir bodies
    either (\e -> hPutStrLn stderr ("stm-scope: no build facts (" <> e <> "): payload words, not field names")) (const (pure ())) facts
    pure p { pFacts = either (const noFacts) id facts }
