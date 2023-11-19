module Eval where
import GrammarTree

{- Helpers -}

unwrapFunction :: FnTreeNode -> Function
unwrapFunction ftn = case ftn of
    (FnLeafArr _) -> undefined -- TODO exception (expected function, not array)
    _ -> evalFnTree ftn

monApply :: FuncM -> (IdMap, Array) -> (IdMap, Array)
monApply f (i, a) = f i (ArrLeaf a)

dyadApply :: FuncD -> (IdMap -> (IdMap, Array)) -> (IdMap, Array) -> (IdMap, Array)
dyadApply f lf (i, r) = f i' (ArrLeaf l) (ArrLeaf r)
    where (i', l) = lf i

atopMM :: FuncM -> FuncM -> FuncM -- (f g)Y = f g Y
atopMM f g i y = monApply f $ g i y

atopMD :: FuncM -> FuncD -> FuncD -- X(f g)Y = f X g Y
atopMD f g i x y = monApply f $ g i x y

forkADD :: ArrTreeNode -> FuncD -> FuncD -> FuncD -- X(Z g h)Y = Z g X h Y
forkADD z g h i x y = dyadApply g (\i' -> (z, i')) $ h i x y

forkADM :: ArrTreeNode -> FuncD -> FuncM -> FuncD -- (X g h)Y = X g h Y
forkADM x g h i y = dyadApply g (\i' -> (x, i')) $ h i y

forkDDD :: FuncD -> FuncD -> FuncD -> FuncD -- X(f g h)Y = (X f Y) g (X h Y)
forkDDD f g h i x y = dyadApply g (\i' -> f i' x y) $ h i x y

forkMDM :: FuncM -> FuncD -> FuncM -> FuncM -- (f g h)Y = (f Y) g (h Y)
forkMDM f g h i y = dyadApply g (\i' -> f i' y) $ h i y

{- Eval Atop/Fork -}

atop :: FnTreeNode -> FnTreeNode -> Function
atop f' g' = case (f, g) of
    (MonFn _ fF, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonFn _ fF, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonFn _ fF, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    (MonDyadFn _ fF _, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonDyadFn _ fF _, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    _ -> undefined -- TODO exception (internal)
    where f = unwrapFunction f'
          g = unwrapFunction g'
          dName = "der_atop"

fork :: FnTreeNode -> FnTreeNode -> FnTreeNode -> Function
fork f g h = case (f, g, h) of
    (_, FnLeafArr, _) -> undefined -- TODO exception (internal)
    (_, _, FnLeafArr) -> undefined -- TODO exception (internal)
    (FnLeafArr fA) -> case (evalArrTree g, evalArrTree h) of
        (DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
        (DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
        (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
        (MonDyadFn _ _ gF, MonFn _ hF) -> DyadFn dName (forkADM fA gF hF)
        (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkADM fA gF hMF) (forkADD fA gF hDF)
        _ -> undefined
    _ -> case (evalFnTree f, evalFnTree g, evalFnTree h) of
        (DyadFn _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (DyadFn _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonFn _ fF, DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonFn _ fF, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonDyadFn _ fMF fDF, DyadFn _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkMDM fMF gF hMF) (forkDDD fDf gF hDF)
        (MonDyadFn _ fMF fDF, MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
        (MonDyadFn _ _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonDyadFn _ _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonDyadFn _ fF _, DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonDyadFn _ fF _, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (DyadFn _ fF, DyadFn _ gF, MonDyadFn _ _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (DyadFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonFn _ fF, DyadFn _ gF, MonDyadFn _ hF _) -> MonFn dName (forkMDM fF gF hF)
        (MonFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ hF _) -> MonFn dName (forkMDM fF gF hF)
    where dName = "der_fork"

{- Eval Tree Fns -}

evalArrTree :: IdMap -> ArrTreeNode -> (IdMap, Array)
evalArrTree idm (ArrLeaf a) = (idm a)
evalArrTree idm (ArrInternalMonFn ft at) = case evalFnTree ft of
    (MonFn _ f) -> f idm at
    (MonDyadFn _ f _) -> f idm at
    _ -> undefined -- TODO exception
evalArrTree idm (ArrInternalDyadFn ft at1 at2) = case evalFnTree ft of
    (DyadFn _ f) -> f idm at1 at2
    (MonDyadFn _ _ f) -> f idm at1 at2
    _ -> undefined -- TODO exception
evalArrTree idm (ArrInternalSubscript a is) = undefined -- TODO implement

evalFnTree :: FnTreeNode -> Function
evalFnTree (FnLeafFn f) = ft f
evalFnTree (FnLeafArr _) = undefined -- TODO internal error (?)
evalFnTree (FnInternalMonOp op ft) = case op of
    (MonOp _ o) -> o idm ft
    (DyadOp _ _) -> undefined
evalFnTree (FnInternalDyadOp op ft1 ft2) = case op of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> o idm ft1 ft2
evalFnTree (FnInternalAtop ft1 ft2) = atop ft1 ft2
evalFnTree (FnInternalFork ft1 ft2 ft3) = fork ft1 ft2 ft3
