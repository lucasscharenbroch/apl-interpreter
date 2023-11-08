module Eval where
import GrammarTree
import Glyphs

atop :: FnTreeNode -> FnTreeNode -> Function
atop f' g' = case (f, g) of
    ((MonFn fName fF), (MonFn gName gF)) -> MonFn derName (fF . ArrLeaf . gF)
        where derName = "derived (" ++ fName ++ ") (" ++ gName ++ ")"
    ((MonFn fName fF), (DyadFn gName gF)) -> DyadFn derName (\x y -> fF . ArrLeaf $ gF x y)
        where derName = "derived (" ++ fName ++ ") (" ++ gName ++ ")"
    _ -> undefined -- TODO exception (?)
    where f = case evalFnTree f' of
              (Left fn) -> fn
              (Right _) -> undefined -- TODO exception
          g = case evalFnTree f' of
              (Left fn) -> fn
              (Right _) -> undefined -- TODO exception

fork :: FnTreeNode -> FnTreeNode -> FnTreeNode -> Function
fork f g h = case (evalFnTree f, evalFnTree g, evalFnTree h) of
    (Right fA, Left (DyadFn _ gF), Left (DyadFn _ hF)) -> DyadFn "der_fork" (\x y -> gF z (ArrLeaf $ hF x y))
        where z = ArrLeaf fA
    (Left (DyadFn _ fF), Left (DyadFn _ gF), Left (DyadFn _ hF)) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
    (Right fA, Left (DyadFn _ gF), Left (MonFn _ hF)) -> MonFn "der_fork" (\y -> gF x (ArrLeaf $ hF y))
        where x = ArrLeaf fA
    (Left (MonFn _ fF), Left (DyadFn _ gF), Left (MonFn _ hF)) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x) (ArrLeaf $ hF y))
    _ -> undefined -- TODO exception

evalArrTree :: ArrTreeNode -> Array
evalArrTree (ArrLeaf a) = a
evalArrTree (ArrInternalMonFn ft at) = case evalFnTree ft of
    (Left (MonFn _ f)) -> f at
    (Left (MonDyadFn _ f _)) -> f at
    _ -> undefined -- TODO exception
evalArrTree (ArrInternalDyadFn ft at1 at2) = case evalFnTree ft of
    (Left (DyadFn _ f)) -> f at1 at2
    (Left (MonDyadFn _ _ f)) -> f at1 at2
    _ -> undefined -- TODO exception

evalFnTree :: FnTreeNode -> Either Function Array
evalFnTree (FnLeafFn f) = Left f
evalFnTree (FnLeafArr at) = Right $ evalArrTree at
evalFnTree (FnInternalMonOp op ft) = case op of
    (MonOp _ o) -> Left $ o ft
    (DyadOp _ _) -> undefined
evalFnTree (FnInternalDyadOp op ft1 ft2) = case op of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> Left $ o ft1 ft2
evalFnTree (FnInternalAtop ft1 ft2) = Left $ atop ft1 ft2
evalFnTree (FnInternalFork ft1 ft2 ft3) = Left $ fork ft1 ft2 ft3
