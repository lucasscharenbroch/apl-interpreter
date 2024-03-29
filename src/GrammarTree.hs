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

{- Array Helpers (low-level) -}

at :: Array -> Int -> Scalar
at a i = (cells a) A.! i

arrMod :: Array -> [(Int, Scalar)] -> Array
arrMod a l = a { cells = (cells a) A.// l }

shapedArrFromList :: [Int] -> [Scalar] -> Array
shapedArrFromList shape cells
    | size /= length cells = undefined
    | otherwise = Array {
                          shape = shape
                        , cells = A.listArray (0, size - 1) cells
                        }
    where size = foldr (*) 1 shape

listToArr :: [Scalar] -> Array
listToArr l = shapedArrFromList [length l] l

arrToList :: Array -> [Scalar]
arrToList (Array shape cells) = [cells A.! i | i <- [0..(sz - 1)]]
    where sz = foldr (*) 1 shape

{- Ordering (for sorting) -}

instance Ord Scalar where
    compare x y =  case (x, y) of
        (ScalarNum xn, ScalarNum yn) -> compare xn yn
        (ScalarNum xn, ScalarCh yn) -> LT
        (ScalarCh xn, ScalarNum yn) -> GT
        (ScalarCh xc, ScalarCh yc) -> compare xc yc
        (ScalarArr a, ScalarArr b) -> compare a b
        (ScalarArr a, s) -> compare a (listToArr [s])
        (s, ScalarArr a) -> compare (listToArr [s]) a

instance Ord Array where
    compare x y = compare (arrToList x) (arrToList y)

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
                       , fnOnAxisM :: Maybe (Double -> FuncM)
                       , fnInverseM :: Maybe (FuncM)
                       , fnCanSelectM :: Bool
                       }

instance FnInfoT FnInfoM where
    fnInfoName = fnNameM
    fnInfoNamePad = fnNamePadM

data FnInfoD = FnInfoD {
                         fnNameD :: String
                       , fnNamePadD :: Int
                       , fnIdD :: Maybe Scalar
                       , fnOnAxisD :: Maybe (Double -> FuncD)
                       , fnInverseD :: Maybe (FuncD)
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
                       , fnOnAxisAM :: Maybe (Double -> FuncM)
                       , fnOnAxisAD :: Maybe (Double -> FuncD)
                       , fnInverseAM :: Maybe (FuncM)
                       , fnInverseAD :: Maybe (FuncD)
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
              | AmbivFn FnInfoA FuncM FuncD

type OpM = (Either Array Function) -> StateT IdMap IO Function
type OpD = (Either Array Function) -> OpM

data Operator = MonOp String OpM
              | DyadOp String OpD

type NiladicFn = StateT IdMap IO Array

{- Tree Nodes -}

-- "function tree": a tree that makes up a derived function:
-- the internal nodes are operators, and the leaves are functions or (derived) arrays
data FnTreeNode = FnLeafFn Function
                | FnLeafVar String
                | FnLeafArr ArrTreeNode
                | FnInternalMonOp OpTreeNode FnTreeNode
                | FnInternalDyadOp OpTreeNode FnTreeNode FnTreeNode
                | FnInternalAtop FnTreeNode FnTreeNode
                | FnInternalFork FnTreeNode FnTreeNode FnTreeNode
                | FnInternalAssignment String FnTreeNode
                | FnInternalQuadAssignment FnTreeNode
                | FnInternalAxisSpec FnTreeNode ArrTreeNode
                | FnInternalDummyNode FnTreeNode

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeaf Array
                 | ArrLeafVar String
                 | ArrNiladicFn String NiladicFn
                 | ArrInternalSubscript ArrTreeNode [Maybe ArrTreeNode]
                 | ArrInternalAssignment String ArrTreeNode
                 | ArrInternalModAssignment String FnTreeNode ArrTreeNode
                 | ArrInternalSelAssignment ArrTreeNode ArrTreeNode
                 | ArrInternalQuadAssignment ArrTreeNode
                 | ArrInternalQuadIdAssignment String ArrTreeNode
                 | ArrInternalMonFn FnTreeNode ArrTreeNode
                 | ArrInternalDyadFn FnTreeNode ArrTreeNode ArrTreeNode
                 | ArrInternalImplCat ArrTreeNode ArrTreeNode
                 | ArrInternalImplGroup ArrTreeNode

data OpTreeNode = OpLeaf Operator -- linked-list of assignments
                | OpLeafVar String
                | OpInternalAssignment String OpTreeNode
                | OpInternalQuadAssignment OpTreeNode
                | OpInternalDummyNode OpTreeNode -- does nothing except prevent an assignment
                                                 -- from being matched (in order to allow printing)

opTreeIsMonadic :: IdMap -> OpTreeNode -> Bool
opTreeIsMonadic _ (OpLeaf o) = case o of
    MonOp _ _ -> True
    DyadOp _ _ -> False
opTreeIsMonadic idm (OpLeafVar id) = case mapLookup id idm of
    Just (IdOp o) -> case o of
        MonOp _ _ -> True
        DyadOp _ _ -> False
    Just (IdDop _ isDy) -> not isDy
    Nothing -> undefined
opTreeIsMonadic idm (OpInternalAssignment _ otn) = opTreeIsMonadic idm otn
opTreeIsMonadic idm (OpInternalQuadAssignment otn) = opTreeIsMonadic idm otn
opTreeIsMonadic idm (OpInternalDummyNode otn) = opTreeIsMonadic idm otn

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
