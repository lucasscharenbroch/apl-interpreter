module GrammarTree where
import qualified Data.Array as A
import Lex
import qualified Data.Map as Map
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

{- Functions and Operators -}

type FuncM = Array -> StateT IdMap IO Array
type FuncD = Array -> FuncM

data Function = MonFn String FuncM
              | DyadFn String FuncD
              | MonDyadFn String FuncM FuncD

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
                 | ArrInternalSubscript ArrTreeNode [ArrTreeNode]
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
