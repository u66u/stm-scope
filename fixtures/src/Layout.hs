-- Not a pathology: prints, for each test type, which field each payload word
-- of a real heap object holds. check.sh compares this with what the facts
-- helper derives from the .hi files (spike S7).
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Word (Word64)
import GHC.Exts.Heap
import GHC.Float (castDoubleToWord64)

data A1 = A1 { a :: !Int, b :: Int, c :: !Word }
data Pair = Pair !Int !Int
data A2 = A2 { inner :: {-# UNPACK #-} !Pair, z :: !Int, w :: Int }
newtype Count = Count Int
data A3 = A3 { n :: !Count, s :: Integer }
data A4 = A4 { l1 :: Int, l2 :: Integer }
data A5 = A5 !Int Int {-# UNPACK #-} !Double
data A6 = A6 { m1 :: !Char, m2 :: Int, m3 :: !Int64 }
data A8 = A8x { x :: !Int } | A8y { y :: Int, x :: !Int }
data A9 = A9 { d :: {-# UNPACK #-} !Double, e :: !Integer, k :: !Word64 }
data A10 = A10 { p :: {-# NOUNPACK #-} !Int, q :: Int }

-- | Every field holds a distinct number; a word is named by the number it
-- holds, or for a pointer by the number in the object it points to.
observe :: a -> [(Word, String)] -> IO ()
observe v names = do
  ConstrClosure {ptrArgs = ps, dataArgs = ds, pkg = pk, modl = m, name = con} <- getBoxedClosureData (asBox v)
  pointees <- mapM (fmap firstWord . getBoxedClosureData) ps
  let name i = maybe "?" id (lookup i names)
  putStrLn (intercalate "\t" ["obs", pk <> ":" <> m <> "." <> con, intercalate "," (map name (pointees <> ds))])
  where firstWord = \case ConstrClosure {dataArgs = i : _} -> i; _ -> 0

main :: IO ()
main = do
  let dw = fromIntegral . castDoubleToWord64
  observe (A1 101 102 103) [(101, "a"), (102, "b"), (103, "c")]
  observe (A2 (Pair 201 202) 203 204) [(201, "inner"), (202, "inner"), (203, "z"), (204, "w")]
  observe (A3 (Count 301) 302) [(301, "n"), (302, "s")]
  observe (A4 401 402) [(401, "l1"), (402, "l2")]
  observe (A5 501 502 5.5) [(501, "#1"), (502, "#2"), (dw 5.5, "#3")]
  observe (A6 'X' 602 603) [(88, "m1"), (602, "m2"), (603, "m3")]
  observe (A8x 801) [(801, "x")]
  observe (A8y 802 803) [(802, "y"), (803, "x")]
  observe (A9 9.5 902 903) [(dw 9.5, "d"), (902, "e"), (903, "k")]
  observe (A10 1001 1002) [(1001, "p"), (1002, "q")]
