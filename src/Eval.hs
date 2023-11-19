module Eval where
import GrammarTree

atop :: FnTreeNode -> FnTreeNode -> Function
atop f' g' = case (f, g) of
    ((MonFn _ fF), (MonFn _ gF)) -> MonFn "der_atop" (fF . ArrLeaf . gF)
    ((MonFn _ fF), (DyadFn _ gF)) -> DyadFn "der_atop" (\x y -> fF . ArrLeaf $ gF x y)
    ((MonFn _ fF), (MonDyadFn _ gMF gDF)) -> MonDyadFn "der_atop" (fF . ArrLeaf . gMF) (\x y -> fF . ArrLeaf $ gDF x y)
    ((MonDyadFn _ fF _), (MonFn _ gF)) -> MonFn "der_atop" (fF . ArrLeaf . gF)
    ((MonDyadFn _ fF _), (DyadFn _ gF)) -> DyadFn "der_atop" (\x y -> fF . ArrLeaf $ gF x y)
    ((MonDyadFn _ fF _), (MonDyadFn _ gMF gDF)) -> MonDyadFn "der_atop" (fF . ArrLeaf . gMF) (\x y -> fF . ArrLeaf $ gDF x y)
    _ -> undefined -- TODO exception (?)
    where f = case evalFnTree f' of
              (Left fn) -> fn
              (Right _) -> undefined
          g = case evalFnTree f' of
              (Left fn) -> fn
              (Right _) -> undefined

fork :: FnTreeNode -> FnTreeNode -> FnTreeNode -> Function
fork f g h = case (evalFnTree f, evalFnTree g, evalFnTree h) of
    (Right fA, Left g', Left h') -> case (g', h') of
        (DyadFn _ gF, DyadFn _ hF) -> DyadFn "der_train" (\x y -> gF (ArrLeaf fA) (ArrLeaf $ hF x y))
        (DyadFn _ gF, MonFn _ hF) -> DyadFn "der_train" (\x y -> gF (ArrLeaf fA) (ArrLeaf $ hF y))
        (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn "der_train" (\x y -> gF (ArrLeaf fA) (ArrLeaf $ hF x y))
        (MonDyadFn _ _ gF, MonFn _ hF) -> DyadFn "der_train" (\x y -> gF (ArrLeaf fA) (ArrLeaf $ hF y))
        (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn "der_train" monF dyF
            where monF = (\y -> gF (ArrLeaf fA) (ArrLeaf $ hMF y))
                  dyF = (\x y -> gF (ArrLeaf fA) (ArrLeaf $ hDF x y))
        _ -> undefined
    (Left f', Left g', Left h') -> case (f', g', h') of
        (DyadFn _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (DyadFn _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (MonFn _ fF, DyadFn _ gF, MonFn _ hF) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
        (MonFn _ fF, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
        (MonDyadFn _ fMF fDF, DyadFn _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn "der_form" monF dyF
            where monF = (\y -> gF (ArrLeaf $ fMF y) (ArrLeaf $ hMF y))
                  dyF = (\x y -> gF (ArrLeaf $ fDF x y) (ArrLeaf $ hDF x y))
        (MonDyadFn _ fMF fDF, MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn "der_form" monF dyF
            where monF = (\y -> gF (ArrLeaf $ fMF y) (ArrLeaf $ hMF y))
                  dyF = (\x y -> gF (ArrLeaf $ fDF x y) (ArrLeaf $ hDF x y))
        (MonDyadFn _ _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (MonDyadFn _ _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (MonDyadFn _ fF _, DyadFn _ gF, MonFn _ hF) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
        (MonDyadFn _ fF _, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
        (DyadFn _ fF, DyadFn _ gF, MonDyadFn _ _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (DyadFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ _ hF) -> DyadFn "der_fork" (\x y -> gF (ArrLeaf $ fF x y) (ArrLeaf $ hF x y))
        (MonFn _ fF, DyadFn _ gF, MonDyadFn _ hF _) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
        (MonFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ hF _) -> MonFn "der_fork" (\y -> gF (ArrLeaf $ fF y) (ArrLeaf $ hF y))
    _ -> undefined

evalArrTree :: (IdMap, ArrTreeNode) -> Array
evalArrTree (idm, ArrLeaf a) = a
evalArrTree (idm, ArrInternalMonFn ft at) = case evalFnTree (idm, ft) of
    (Left (MonFn _ f)) -> f idm at
    (Left (MonDyadFn _ f _)) -> f idm at
    _ -> undefined -- TODO exception
evalArrTree (idm, ArrInternalDyadFn ft at1 at2) = case evalFnTree (idm, ft) of
    (Left (DyadFn _ f)) -> f idm at1 at2
    (Left (MonDyadFn _ _ f)) -> f idm at1 at2
    _ -> undefined -- TODO exception
evalArrTree (idm, ArrInternalSubscript a is) = undefined -- TODO implement

evalFnTree :: (IdMap, FnTreeNode) -> Either Function Array
evalFnTree (_, FnLeafFn f) = Left f
evalFnTree (idm, FnLeafArr at) = Right $ evalArrTree (idm, at)
evalFnTree (idm, FnInternalMonOp op ft) = case op of
    (MonOp _ o) -> Left $ o idm ft
    (DyadOp _ _) -> undefined
evalFnTree (idm, FnInternalDyadOp op ft1 ft2) = case op of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> Left $ o idm ft1 ft2
evalFnTree (_, FnInternalAtop ft1 ft2) = Left $ atop ft1 ft2
evalFnTree (_, FnInternalFork ft1 ft2 ft3) = Left $ fork ft1 ft2 ft3
