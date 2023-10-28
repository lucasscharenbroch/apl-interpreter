module GrammarTree where
import qualified Data.Array as A

data Scalar = ScalarNum Double
            | ScalarCh Char
            | ScalarArr Array

data Array = Array {
               shape :: [Int]
             , cells :: A.Array Int Scalar
             }

arrFromList :: [Int] -> [Scalar] -> Array
arrFromList shape cells = Array {
                           shape = shape
                         , cells = A.listArray (0, size - 1) cells
                         }
    where size = foldr (*) 1 shape

data Applyable a r = Applyable {
                     name :: String
                   , isM :: Bool
                   , isD :: Bool
                   , mApply :: a -> r
                   , dApply :: a -> a -> r
                   }

type Function = Applyable Array Array
type Operator = Applyable (Either Array Function) Function

-- "function tree": a tree that makes up a derived function:
-- the internal nodes are operators, and the leaves are functions
data FnTreeNode = FnLeaf Function
                | FnInternalOp Operator FnTreeNode (Maybe FnTreeNode) -- 1 or 2 children
                | FnInternalTrain FnTreeNode FnTreeNode (Maybe FnTreeNode) -- 2 or 3 children

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeaf Array
                 | ArrInternalFn FnTreeNode (Maybe ArrTreeNode) ArrTreeNode -- 1 or 2 children
