module GrammarTree where
import qualified Data.Array as A
import Lex
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

{- Scalar -}

data Scalar = ScalarNum Double
            | ScalarCh Char
            | ScalarArr Array
    deriving (Eq)

{- Arrays -}

data Array = Array {
               shape :: [Int]
             , cells :: A.Array Int Scalar
             }
    deriving (Eq)

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n x = [take n x] ++ groupBy n (drop n x)

isScalarArr :: Scalar -> Bool
isScalarArr (ScalarArr _) = True
isScalarArr _ = False

isScalarCh :: Scalar -> Bool
isScalarCh (ScalarCh _) = True
isScalarCh _ = False

{- Array Helpers -}

at :: Array -> Int -> Scalar
at a i = (cells a) A.! i

atl :: Array -> [Int] -> Scalar
atl a is = (cells a) A.! i
    where i = sum $ zipWith (*) (tail . scanr (*) 1 . shape $ a) is

shapedArrFromList :: [Int] -> [Scalar] -> Array
shapedArrFromList shape cells = Array {
                                  shape = shape
                                , cells = A.listArray (0, size - 1) cells
                                }
    where size = foldr (*) 1 shape

arrFromList :: [Scalar] -> Array
arrFromList l = shapedArrFromList [length l] l

arrToList :: Array -> [Scalar]
arrToList (Array shape cells) = [cells A.! i | i <- [0..(sz - 1)]]
    where sz = foldr (*) 1 shape

arrCat :: Array -> Array -> Array
arrCat x y = arrFromList $ (arrToList x) ++ (arrToList y)

arrCons :: Scalar -> Array -> Array
arrCons x y = arrFromList $ x : (arrToList y)

arrZipWith :: (Scalar -> Scalar -> Scalar) -> Array -> Array -> Array
arrZipWith f x y
    | xsz /= ysz = undefined
    | otherwise = shapedArrFromList (shape x) [(x `at` i) `f` (y `at` i) | i <- [0..xsz]]
    where xsz = foldr (*) 1 $ shape x
          ysz = foldr (*) 1 $ shape y

arrMap :: (Scalar -> Scalar) -> Array -> Array
arrMap f a = shapedArrFromList (shape a) . map (f) . arrToList $ a

arrRank :: Array -> Int
arrRank a = length . shape $ a

arrIndex :: Array -> [Int] -> Scalar
arrIndex a is = a `at` sum (zipWith (*) is indexMod)
    where indexMod = reverse . init . scanl (*) 1 . reverse . shape $ a

{- Functions and Operators -}

type FuncM = Array -> StateT IdMap IO Array
type FuncD = Array -> FuncM

class FnInfoT t where
    fnInfoName :: t -> String
    fnInfoNamePad :: t -> Int

data FnInfoM = FnInfoM {
                         fnNameM :: String
                       , fnNamePadM :: Int
                       , fnIdM :: Maybe Scalar
                       , fnOnAxisM :: Maybe (Int -> FuncM)
                       , fnInverseM :: Maybe (Int -> FuncM)
                       , fnCanSelectM :: Bool
                       }

instance FnInfoT FnInfoM where
    fnInfoName = fnNameM
    fnInfoNamePad = fnNamePadM

data FnInfoD = FnInfoD {
                         fnNameD :: String
                       , fnNamePadD :: Int
                       , fnIdD :: Maybe Scalar
                       , fnOnAxisD :: Maybe (Int -> FuncD)
                       , fnInverseD :: Maybe (Int -> FuncD)
                       , fnCanSelectD :: Bool
                       }

instance FnInfoT FnInfoD where
    fnInfoName = fnNameD
    fnInfoNamePad = fnNamePadD

data FnInfoA = FnInfoA {
                         fnNameA :: String
                       , fnNamePadA :: Int
                       , fnIdAM :: Maybe Scalar
                       , fnIdAD :: Maybe Scalar
                       , fnOnAxisAM :: Maybe (Int -> FuncM)
                       , fnOnAxisAD :: Maybe (Int -> FuncD)
                       , fnInverseAM :: Maybe (Int -> FuncD)
                       , fnInverseAD :: Maybe (Int -> FuncD)
                       , fnCanSelectAM :: Bool
                       , fnCanSelectAD :: Bool
                       }

instance FnInfoT FnInfoA where
    fnInfoName = fnNameA
    fnInfoNamePad = fnNamePadA

defFnInfoM = FnInfoM { -- default function info (monadic)
                       fnNameM = "(default function name)"
                     , fnNamePadM = 0
                     , fnIdM = Nothing
                     , fnOnAxisM = Nothing
                     , fnInverseM = Nothing
                     , fnCanSelectM = False
                     }

defFnInfoD = FnInfoD { -- default function info (dyadic)
                       fnNameD = "(default function name)"
                     , fnNamePadD = 0
                     , fnIdD = Nothing
                     , fnOnAxisD = Nothing
                     , fnInverseD = Nothing
                     , fnCanSelectD = False
                     }

defFnInfoA = FnInfoA { -- default function info (ambivalent)
                       fnNameA = "(default function name)"
                     , fnNamePadA = 0
                     , fnIdAM = Nothing
                     , fnIdAD = Nothing
                     , fnOnAxisAM = Nothing
                     , fnOnAxisAD = Nothing
                     , fnInverseAM = Nothing
                     , fnInverseAD = Nothing
                     , fnCanSelectAM = False
                     , fnCanSelectAD = False
                     }

data Function = MonFn FnInfoM FuncM
              | DyadFn FnInfoD FuncD
              | MonDyadFn FnInfoA FuncM FuncD

type OpM = (Either Array Function) -> StateT IdMap IO Function
type OpD = (Either Array Function) -> OpM

data Operator = MonOp String OpM
              | DyadOp String OpD

type NiladicFn = StateT IdMap IO Array

{- Tree Nodes -}

-- "function tree": a tree that makes up a derived function:
-- the internal nodes are operators, and the leaves are functions or (derived) arrays
data FnTreeNode = FnLeafFn Function
                | FnLeafArr ArrTreeNode
                | FnInternalMonOp OpTreeNode FnTreeNode
                | FnInternalDyadOp OpTreeNode FnTreeNode FnTreeNode
                | FnInternalAtop FnTreeNode FnTreeNode
                | FnInternalFork FnTreeNode FnTreeNode FnTreeNode
                | FnInternalAssignment String FnTreeNode
                | FnInternalQuadAssignment FnTreeNode
                | FnInternalDummyNode FnTreeNode

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeaf Array
                 | ArrNiladicFn String NiladicFn
                 | ArrInternalSubscript ArrTreeNode [Maybe ArrTreeNode]
                 | ArrInternalAssignment String ArrTreeNode
                 | ArrInternalModAssignment String FnTreeNode ArrTreeNode
                 | ArrInternalQuadAssignment ArrTreeNode
                 | ArrInternalQuadIdAssignment String ArrTreeNode
                 | ArrInternalMonFn FnTreeNode ArrTreeNode
                 | ArrInternalDyadFn FnTreeNode ArrTreeNode ArrTreeNode
                 | ArrInternalImplCat ArrTreeNode ArrTreeNode

data OpTreeNode = OpLeaf Operator -- not really a tree, more like a linked-list of assignments
                | OpInternalAssignment String OpTreeNode
                | OpInternalQuadAssignment OpTreeNode
                | OpInternalDummyNode OpTreeNode -- does nothing except prevent an assignment
                                                 -- from being matched (in order to allow printing)

unwrapOpTree :: OpTreeNode -> Operator
unwrapOpTree (OpLeaf o) = o
unwrapOpTree (OpInternalAssignment _ otn) = unwrapOpTree otn
unwrapOpTree (OpInternalQuadAssignment otn) = unwrapOpTree otn
unwrapOpTree (OpInternalDummyNode otn) = unwrapOpTree otn

{- Id Map -}

data IdEntry = IdArr Array
             | IdFn Function
             | IdOp Operator
             | IdDfn [Token]
             | IdDop [Token] Bool -- toks, is_dyadic
             | IdDerDfn [Token] (Maybe IdEntry) (Maybe IdEntry) (Maybe IdEntry) -- toks, ⍺⍺, ⍵⍵, ∇∇

type IdMap = Map.Map String IdEntry

emptyIdMap :: IdMap
emptyIdMap = Map.empty

mapLookup :: String -> IdMap -> Maybe IdEntry
mapLookup = Map.lookup

mapInsert :: String -> IdEntry -> IdMap -> IdMap
mapInsert = Map.insert

mapDelete :: String -> IdMap -> IdMap
mapDelete = Map.delete
