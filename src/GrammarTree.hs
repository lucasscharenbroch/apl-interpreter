module GrammarTree where
import qualified Data.Array as A

data Scalar = ScalarNum Double
            | ScalarCh Char
            | ScalarArr ArrTreeNode

data Array = Array {
               shape :: [Int]
             , cells :: A.Array Int Scalar
             }

shapedArrFromList :: [Int] -> [Scalar] -> Array
shapedArrFromList shape cells = Array {
                                  shape = shape
                                , cells = A.listArray (0, size - 1) cells
                                }
    where size = foldr (*) 1 shape

arrFromList :: [Scalar] -> Array
arrFromList l = shapedArrFromList [length l] l

-- "function tree": a tree that makes up a derived function:
-- the internal nodes are operators, and the leaves are functions
data FnTreeNode = FnLeaf String
                | FnInternalOp String FnTreeNode (Maybe FnTreeNode) -- 1 or 2 children
                | FnInternalTrain FnTreeNode FnTreeNode (Maybe FnTreeNode) -- 2 or 3 children

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeaf Array
                 | ArrInternalFn FnTreeNode (Maybe ArrTreeNode) ArrTreeNode -- 1 or 2 children

mkMonFnCall :: FnTreeNode -> ArrTreeNode -> ArrTreeNode
mkMonFnCall fn arg = ArrInternalFn fn Nothing arg

mkDyadFnCall :: FnTreeNode -> ArrTreeNode -> ArrTreeNode -> ArrTreeNode
mkDyadFnCall fn arg1 arg2 = ArrInternalFn fn (Just arg1) arg2