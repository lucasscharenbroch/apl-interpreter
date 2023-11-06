module Eval where
import GrammarTree
import Glyphs

{-
evalScalar :: Scalar -> Scalar
evalScalar (ScalarArr a) = case evalArrTree a of
    (Right a'@(Array [1] _)) -> head . arrToList $ a'
    (Right a') -> ScalarArr $ ArrLeaf a'
    (Left s) -> s
evalScalar x = x

-- recursively evaluate array's elements, then flatten the resulting array
-- assume array is 1-dimensional (the parser only produces 1d arrays)
evalArr :: Array -> Either Scalar Array
evalArr a = case map (evalScalar) (arrToList a) of
    (ScalarArr (ArrLeaf a)):[] -> Right a
    s:[] -> Left s
    ss -> Right $ arrFromList ss

-- TODO add id map, io and exceptions
evalArrTree :: ArrTreeNode -> Either Scalar Array
evalArrTree (ArrLeaf a) = evalArr a
evalArrTree (ArrInternalMonFn fn arg) = case evalFnTree fn of
    (MonFn _ f) -> evalArrTree $ f arg'
    (MonDyadFn _ f _) -> evalArrTree $ f arg'
    where arg' = unwrap evalArrTree arg

evalFnTree :: FnTreeNode -> Function
evalFnTree _ = fPlus
-}
