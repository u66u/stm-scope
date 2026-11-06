{-# LANGUAGE CPP, LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | stm-scope-facts: what the runtime cannot see, read from a build's .hi and
-- .hie files. Both formats belong to one GHC, so `stm-scope` compiles this
-- file with the project's own compiler (boot packages only) and runs it at
-- report time.
--
--   stm-scope-facts BUILDDIR < queries > answers              (tab separated)
--
--   query   site <id> <module> <file> <span>    a transaction body, as IPE names it
--   answers comp <id> <component>               the build component that holds it
--           miss <id>                           not found
--           con  <component> <condesc> <tycon> <ptrs> <nptrs> <field of each payload word, comma separated>
--           read <id> <tycon> <fields, comma separated, or *>
--           tvar <id> <tycon> <name> <file:line>
--
-- 'read' over-approximates which fields of a record a site may read, in its
-- body and in every local function it reaches. A record value that goes
-- anywhere the analysis does not follow (a library function, a class method,
-- a constructor, a return, a store) counts as read whole: a split is never
-- recommended on the strength of a read we failed to see.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Array ((!))
import Data.ByteString.Char8 qualified as BC
import Data.IORef
import Data.List (isSuffixOf, partition)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Monoid (Any (..))
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as S
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.Info (fullCompilerVersion)

import GHC.Data.FastString (unpackFS)
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (..), readBinIface)
import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types
import GHC.Iface.Syntax
import GHC.Platform (genericPlatform)
import GHC.Platform.Profile (Profile (..))
import GHC.Types.FieldLabel (FieldLabel (..))
import GHC.Types.Name
import GHC.Types.Name.Cache (NameCache, initNameCache)
import GHC.Types.SrcLoc
import GHC.Unit.Module (Module, moduleName, moduleNameString, moduleUnit)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_module)
import GHC.Unit.Types (unitString)
#if __GLASGOW_HASKELL__ < 908
import Language.Haskell.Syntax.Basic (field_label)
#endif

main :: IO ()
main = getArgs >>= \case
  [dir] -> do
    nc <- nameCache
    comps <- components nc dir
    queries <- mapMaybe query . lines <$> getContents
    done <- newIORef S.empty
    forM_ queries $ \(sid, md, file, span') ->
      case [(c, h) | c <- comps, h <- cHies c, modName (hie_module h) == md, file `isSuffixOf` hie_hs_file h] of
        (c, h) : _ | Just node <- locate h =<< parseSpan span' -> do
          let ix = index (c : [s | s <- comps, cShared s, cRoot s /= cRoot c])
          out ["comp", sid, cRoot c]
          first <- atomicModifyIORef' done (\s -> (S.insert (cRoot c) s, S.notMember (cRoot c) s))
          when first $ mapM_ (out . (["con", cRoot c] <>)) (layouts ix)
          Acc rs tvars _ <- site ix h node
          sequence_ [out ["read", sid, key tc, maybe "*" (commas . S.toList) fs] | (tc, fs) <- M.toList rs]
          sequence_ [out ["tvar", sid, key tc, occ n, loc n] | (tc, n) <- S.toList tvars]
        _ -> out ["miss", sid]
  _ -> hPutStrLn stderr "usage: stm-scope-facts BUILDDIR < queries"
  where
    out = putStrLn . tabs
    query l = case splitOn '\t' l of
      ["site", i, m, f, s] -> Just (i, m, f, s)
      _ -> Nothing

------------------------------------------------------------------------------
-- Loading

-- | One build component (a library, an executable, ...): every module of it,
-- found under one directory. Executables all live in unit "main", so their
-- names clash across components and each is analysed on its own, together
-- with the libraries ("shared") it may use.
data Comp = Comp { cRoot :: FilePath, cShared :: Bool, cHies :: [HieFile], cIfaces :: [ModIface] }

components :: NameCache -> FilePath -> IO [Comp]
components nc dir = do
  files <- walk dir
  hies <- catMaybes <$> forM [f | f <- files, takeExtension f == ".hie"] (load (fmap hie_file_result . readHieFile nc))
  his  <- catMaybes <$> forM [f | f <- files, takeExtension f == ".hi"] (load (readBinIface profile nc IgnoreHiWay QuietBinIFace))
  let roots = M.fromListWith (<>) $
        [(root f (hie_module h), ([h], [], Any (shared (hie_module h)))) | (f, h) <- hies] <>
        [(root f (mi_module i), ([], [i], Any (shared (mi_module i)))) | (f, i) <- his]
  pure [Comp r s hs is | (r, (hs, is, Any s)) <- M.toList roots]
  where
    profile = Profile genericPlatform S.empty   -- only consulted for way checks, which are off
    load rd f = try (rd f) >>= \case
      Right x -> pure (Just (f, x))
      Left (e :: SomeException) -> Nothing <$ hPutStrLn stderr ("stm-scope-facts: skipping " <> f <> ": " <> show e)
    -- strip the module's own directories: A/B/C.hie for module A.B.C
    root f m = iterate takeDirectory (takeDirectory f) !! (length (splitOn '.' (modName m)) - 1)
    shared m = unitString (moduleUnit m) /= "main"
    -- cabal keeps one tree per compiler (build/ARCH/ghc-X.Y.Z): only ours can be read
    walk d = do
      xs <- filter (\x -> not ("ghc-" `L.isPrefixOf` x) || x == ours) <$> listDirectory d
      concat <$> forM xs (\x -> let p = d </> x in doesDirectoryExist p >>= \isDir -> if isDir then walk p else pure [p])
    ours = "ghc-" <> showVersion fullCompilerVersion

------------------------------------------------------------------------------
-- Index

data Ix = Ix
  { xData   :: M.Map Name [IfaceConDecl]          -- ^ local data types (not newtypes)
  , xNew    :: M.Map Name IfaceType               -- ^ local newtypes, to what they wrap
  , xFields :: M.Map Name (Name, String)          -- ^ selector: its type and label
  , xCons   :: M.Map Name (Name, [String])        -- ^ constructor: its type and its fields' labels
  , xDefs   :: M.Map Name (HieFile, HieAST TypeIndex)  -- ^ local bindings, to follow calls into
  , xMods   :: S.Set Module
  }

index :: [Comp] -> Ix
index cs = Ix {..}
  where
    decls = [d | c <- cs, i <- cIfaces c, (_, d) <- mi_decls i]
    xData = M.fromList [(ifName d, cons) | d@IfaceData {ifCons = IfDataTyCon _ cons} <- decls, not (null cons)]
    xNew  = M.fromList [(ifName d, ty) | d@IfaceData {ifCons = IfNewTyCon IfCon {ifConArgTys = [(_, ty)]}} <- decls]
    xCons = M.fromList [(ifConName c, (tc, labels c)) | (tc, cs') <- M.toList xData, c <- cs']
    xFields = M.fromList [(flSelector fl, (tc, label fl)) | (tc, cs') <- M.toList xData, c <- cs', fl <- ifConFields c]
    xMods = S.fromList ([hie_module h | c <- cs, h <- cHies c] <> [mi_module i | c <- cs, i <- cIfaces c])
    xDefs = M.fromList
      [ (n, (h, node))
      | c <- cs, h <- cHies c, ast <- M.elems (getAsts (hie_asts h)), node <- universe ast
      , "FunBind" `S.member` anns node
      , n <- binders node, M.notMember n xFields ]
    binders node = [n | k <- node : nodeChildren node, (Right n, d) <- M.toList (nodeIdentifiers (info k))
                      , any (\case ValBind {} -> True; MatchBind -> True; _ -> False) (identInfo d)]

labels :: IfaceConDecl -> [String]
labels c = case ifConFields c of
  [] -> ["#" <> show i | i <- [1 .. length (ifConArgTys c)]]
  fs -> map label fs

label :: FieldLabel -> String
#if __GLASGOW_HASKELL__ >= 908
label = occNameString . nameOccName . flSelector
#else
label = unpackFS . field_label . flLabel
#endif

------------------------------------------------------------------------------
-- Layouts: which payload word holds which field. The code generator puts
-- pointers first, then non-pointers, each group in field order, after
-- unpacking (ifConStricts are the bangs as implemented, so -O's unboxing of
-- small strict fields is already decided). Anything not word-sized, or
-- with fields we cannot see into, is left out: the report then says
-- "payload word k", which is honest.

data Word' = P | N deriving Eq

layouts :: Ix -> [[String]]
layouts ix =
  [ [condesc c, key tc, show (length ps), show (length ns), commas (map snd (ps <> ns))]
  | (tc, cs) <- M.toList (xData ix), c <- cs, null (ifConCtxt c), null (ifConEqSpec c)
  , Just ws <- [concat <$> sequence (zipWith3 field (labels c) (ifConStricts c) (map snd (ifConArgTys c)))]
  , let (ps, ns) = partition ((== P) . fst) ws ]
  where
    condesc c = key (ifConName c)
    field l b ty = map (\w -> (w, l)) <$> case b of
      IfNoBang -> boxed ty
      IfStrict -> boxed ty
      _        -> unpacked (8 :: Int) ty
    boxed = \case
      IfaceTyConApp tc _ | isPrim (ifaceTyConName tc) -> prim (occ (ifaceTyConName tc))
      _ -> Just [P]
    prim o | o `elem` ["Int#", "Word#", "Double#", "Char#", "Addr#", "Int64#", "Word64#"] = Just [N]
           | o `elem` ["Float#", "Int8#", "Int16#", "Int32#", "Word8#", "Word16#", "Word32#"] = Nothing
           | otherwise = Just [P]                  -- boxed unlifted: arrays, MutVar#, ...
    unpacked 0 = const Nothing
    unpacked d = \case
      IfaceTyConApp tc _
        | Just ty <- M.lookup n (xNew ix) -> unpacked (d - 1) ty
        | Just [c] <- M.lookup n (xData ix), null (ifConCtxt c) ->
            concat <$> sequence (zipWith (inner d) (ifConStricts c) (map snd (ifConArgTys c)))
        | not (M.member n (xData ix)), occ n `elem` ["Int", "Word", "Char", "Double", "Int64", "Word64", "Ptr", "FunPtr"] -> Just [N]
        where n = ifaceTyConName tc
      _ -> Nothing
    inner d b ty = case b of
      IfNoBang -> boxed ty
      IfStrict -> boxed ty
      _        -> unpacked (d - 1) ty
    isPrim n = maybe False ((== "GHC.Prim") . modName) (nameModule_maybe n)

------------------------------------------------------------------------------
-- What a site reads

data Acc = Acc
  { aReads :: M.Map Name (Maybe (S.Set String))   -- ^ by record type; Nothing = all of it
  , aTVars :: S.Set (Name, Name)                  -- ^ (contents type, name) of TVars it mentions
  , aSeen  :: S.Set (FilePath, Span)
  }

site :: Ix -> HieFile -> HieAST TypeIndex -> IO Acc
site ix h0 node0 = do
  acc <- newIORef (Acc M.empty S.empty S.empty)
  let readF tc f = modifyIORef' acc $ \a -> a { aReads = M.insertWith merge tc (Just (S.singleton f)) (aReads a) }
      whole tc = modifyIORef' acc $ \a -> a { aReads = M.insert tc Nothing (aReads a) }
      merge new old = S.union <$> new <*> old
      visit h n = do
        seen <- aSeen <$> readIORef acc
        let k = (hie_hs_file h, nodeSpan n)
        unless (S.member k seen) $ do
          modifyIORef' acc (\a -> a { aSeen = S.insert k (aSeen a) })
          go h Nothing n

      go h parent n = do
        let ni = info n
            ty = typeOf h
        forM_ (M.toList (nodeIdentifiers ni)) $ \case
          (Left _, _) -> pure ()
          (Right nm, d) -> do
            let ctx = identInfo d
            when (any fieldRead ctx) $ forM_ (M.lookup nm (xFields ix)) (uncurry readF)
            when (Use `S.member` ctx) $ do
              forM_ (M.lookup nm (xDefs ix)) (uncurry visit)
              forM_ (identType d >>= tvarOf ty) $ \tc -> modifyIORef' acc (\a -> a { aTVars = S.insert (tc, nm) (aTVars a) })
              -- a variable holding a record: it must be taken apart where we can see it
              forM_ (identType d >>= recordOf ty) $ \tc ->
                unless (local nm) $ accounted h parent n nm tc >>= \ok -> unless ok (whole tc)
              -- a library function at a record type may read any of it
              when (external nm && occ nm `notElem` plumbing) $
                mapM_ whole (concatMap (mentions ty) (nodeType ni <> maybeToList (identType d)))
            when (EvidenceVarUse `S.member` ctx) $ mapM_ whole (maybe [] (mentions ty) (identType d))
        when ("ConPat" `S.member` anns n) $ conPat n readF
        when ("RecordUpd" `S.member` anns n) $
          -- r { f = x } copies every other field of r
          forM_ (listToMaybe (mapMaybe (recordOf ty) (nodeType ni))) $ \tc -> do
            let assigned = S.fromList [l | k <- universe n, (Right f, d) <- M.toList (nodeIdentifiers (info k))
                                         , any (\case RecField RecFieldAssign _ -> True; _ -> False) (identInfo d)
                                         , Just (_, l) <- [M.lookup f (xFields ix)]]
            mapM_ (readF tc) [l | l <- fieldsOf tc, S.notMember l assigned]
        mapM_ (go h (Just n)) (nodeChildren n)

      -- Where a record-typed variable is used: as the argument of one of its
      -- selectors, of a local function (followed), of `seq`; updated; or
      -- scrutinised (the patterns are seen). Everything else escapes.
      accounted h parent n nm tc = case parent of
        Nothing -> pure False
        Just p
          | has "HsApp", f : rest <- kids, any (same n) rest -> pure (applies (headName f))
          | has "RecordUpd" || has "HsCase", k : _ <- kids, same n k -> pure True
          | has "OpApp", [l, o, r] <- kids -> pure $ case occ <$> headName o of
              Just op | op `elem` ["$", "$!"], same n r -> applies (headName l)
                      | op == "&", same n l           -> applies (headName r)
                      | op == "seq"                   -> True
              _ -> False
          | [k] <- kids, same n k, Just f <- dotField h p nm, f `elem` fieldsOf tc -> True <$ readF tc f
          | otherwise -> pure False
          where kids = nodeChildren p
                has a = a `S.member` anns p
      applies = maybe False (\f -> M.member f (xFields ix) || M.member f (xDefs ix) || occ f == "seq")

      conPat n readF' = case [(tc, ls, k) | k <- nodeChildren n, (Right c, _) <- M.toList (nodeIdentifiers (info k))
                                          , Just (tc, ls) <- [M.lookup c (xCons ix)]] of
        (tc, ls, k) : _ | not (any (S.member "HsFieldBind" . anns) (nodeChildren n)) ->
          sequence_ [readF' tc l | (l, p) <- zip ls (filter (not . same k) (nodeChildren n)), not ("WildPat" `S.member` anns p)]
        _ -> pure ()   -- record syntax: its fields are RecFieldMatch occurrences

      local nm = M.member nm (xDefs ix) || M.member nm (xFields ix)
      external nm = isExternalName nm && maybe True (`S.notMember` xMods ix) (nameModule_maybe nm)
      fieldsOf tc = S.toList (S.fromList (concatMap snd (M.elems (M.filter ((== tc) . fst) (xCons ix)))))
      -- record types (local data types with fields) a type mentions, except inside a TVar
      mentions ty i = case ty i of
        HTyConApp tc (HieArgs as)
          | isTVar (ifaceTyConName tc) -> []
          | otherwise -> [n | let n = ifaceTyConName tc, M.member n (xData ix)] <> concatMap (mentions ty . snd) as
        HAppTy f (HieArgs as) -> mentions ty f <> concatMap (mentions ty . snd) as
        HForAllTy _ t -> mentions ty t
        HFunTy _ a b -> mentions ty a <> mentions ty b
        HQualTy c t -> mentions ty c <> mentions ty t
        HCastTy t -> mentions ty t
        _ -> []
      recordOf ty i = case ty i of
        HTyConApp tc _ | M.member (ifaceTyConName tc) (xData ix) -> Just (ifaceTyConName tc)
        _ -> Nothing
  visit h0 node0
  readIORef acc
  where
    used = \case Use -> True; RecField RecFieldOcc _ -> True; _ -> False
    fieldRead = \case RecField RecFieldOcc _ -> True; RecField RecFieldMatch _ -> True; _ -> False
    typeOf h i = hie_types h ! i
    tvarOf ty i = case ty i of
      HTyConApp tv (HieArgs as@(_ : _)) | isTVar (ifaceTyConName tv), HTyConApp tc _ <- ty (snd (last as)) -> Just (ifaceTyConName tc)
      HFunTy _ _ r  -> tvarOf ty r
      HForAllTy _ t -> tvarOf ty t
      HQualTy _ t   -> tvarOf ty t
      _ -> Nothing
    same a b = nodeSpan a == nodeSpan b
    headName k = case [n | (Right n, d) <- M.toList (nodeIdentifiers (info k)), any used (identInfo d)] of
      n : _ -> Just n
      []    -> headName =<< listToMaybe (nodeChildren k)

-- | Combinators that pass a value along without looking inside it.
plumbing :: [String]
plumbing =
  [ "$", "$!", "&", ".", ">>=", ">>", "=<<", "fmap", "<$>", "<&>", "<*>", "*>", "<*", "seq", "id"
  , "readTVar", "readTVarIO", "writeTVar", "newTVar", "newTVarIO", "modifyTVar", "modifyTVar'"
  , "stateTVar", "swapTVar", "atomically" ]

isTVar :: Name -> Bool
isTVar n = occ n == "TVar" && maybe False (("Conc.Sync" `isSuffixOf`) . modName) (nameModule_maybe n)

-- | OverloadedRecordDot: `r.f` leaves no field occurrence in HIE, only `r`;
-- recover the field from the source text.
dotField :: HieFile -> HieAST a -> Name -> Maybe String
dotField h p nm = do
  let s = nodeSpan p
  unless' (srcSpanStartLine s == srcSpanEndLine s)
  ln <- listToMaybe (drop (srcSpanStartLine s - 1) (BC.lines (hie_hs_src h)))
  let txt = BC.unpack (BC.take (srcSpanEndCol s - srcSpanStartCol s) (BC.drop (srcSpanStartCol s - 1) ln))
  case splitOn '.' txt of
    [v, f] | v == occ nm, not (null f) -> Just f
    _ -> Nothing
  where unless' b = if b then Just () else Nothing

------------------------------------------------------------------------------
-- Spans

-- | IPE spans: "l:c", "l:c1-c2" or "(l1,c1)-(l2,c2)", end column inclusive.
parseSpan :: String -> Maybe (Int, Int, Int, Int)
parseSpan s = case nums s of
  [l, c]          -> Just (l, c, l, c)
  [l, c1, c2]     -> Just (l, c1, l, c2)
  [l1, c1, l2, c2] -> Just (l1, c1, l2, c2)
  _ -> Nothing
  where nums = map read . filter (not . null) . splitWhen (`notElem` ['0' .. '9'])

-- | The node IPE means: same start, same end (HIE ends are exclusive), the
-- outermost if several; failing that, the smallest node that contains it.
locate :: HieFile -> (Int, Int, Int, Int) -> Maybe (HieAST TypeIndex)
locate h (l1, c1, l2, c2) = case exact of
  n : _ -> Just n
  []    -> listToMaybe (reverse inside)
  where
    nodes = concatMap universe (M.elems (getAsts (hie_asts h)))
    start n = (srcSpanStartLine (nodeSpan n), srcSpanStartCol (nodeSpan n))
    end n = (srcSpanEndLine (nodeSpan n), srcSpanEndCol (nodeSpan n))
    exact = [n | n <- nodes, start n == (l1, c1), end n `elem` [(l2, c2 + 1), (l2, c2)]]
    inside = [n | n <- nodes, start n <= (l1, c1), end n >= (l2, c2 + 1)]   -- pre-order: deepest last

------------------------------------------------------------------------------
-- Small things

info :: HieAST TypeIndex -> NodeInfo TypeIndex
info = L.foldl' join (NodeInfo S.empty [] M.empty) . M.elems . getSourcedNodeInfo . sourcedNodeInfo
  where join (NodeInfo a t i) (NodeInfo a' t' i') = NodeInfo (a <> a') (t <> t') (M.unionWith joinId i i')
        joinId (IdentifierDetails t c) (IdentifierDetails t' c') = IdentifierDetails (maybe t' Just t) (c <> c')

anns :: HieAST TypeIndex -> S.Set String
anns = S.map (unpackFS . nodeAnnotConstr) . nodeAnnotations . info

universe :: HieAST a -> [HieAST a]
universe n = n : concatMap universe (nodeChildren n)

key :: Name -> String
key n = maybe "" (\m -> unitString (moduleUnit m) <> ":" <> modName m <> ".") (nameModule_maybe n) <> occ n

occ :: Name -> String
occ = occNameString . nameOccName

modName :: Module -> String
modName = moduleNameString . moduleName

loc :: Name -> String
loc n = case nameSrcSpan n of
  RealSrcSpan s _ -> unpackFS (srcSpanFile s) <> ":" <> show (srcSpanStartLine s)
  _ -> ""

commas :: [String] -> String
commas = foldr1' (\a b -> a <> "," <> b)
  where foldr1' _ [] = ""
        foldr1' f xs = foldr1 f xs

tabs :: [String] -> String
tabs = foldr1 (\a b -> a <> "\t" <> b)

-- | Deprecated from 9.14 in favour of newNameCache, which 9.6 lacks.
nameCache :: IO NameCache
nameCache = initNameCache 'z' []

splitOn :: Char -> String -> [String]
splitOn c = splitWhen (== c)

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case break p s of
  (a, _ : rest) -> a : splitWhen p rest
  (a, [])       -> [a]
