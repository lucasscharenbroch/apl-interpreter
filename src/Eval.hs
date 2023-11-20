module Eval where
import GrammarTree

{- Helpers -}

expectFunction :: IdMap -> FnTreeNode -> (IdMap, Function)
expectFunction i ftn = case ftn of
    (FnLeafArr _) -> undefined -- TODO exception (expected function, not array)
    _ -> evalFnTree i ftn

monApply :: FuncM -> (IdMap, Array) -> (IdMap, Array)
monApply f (i, a) = f i (ArrLeaf a)

dyadApply :: FuncD -> (IdMap -> (IdMap, Array)) -> (IdMap, Array) -> (IdMap, Array)
dyadApply f lf (i, r) = f i' (ArrLeaf l) (ArrLeaf r)
    where (i', l) = lf i

atopMM :: FuncM -> FuncM -> FuncM -- (f g)Y = f g Y
atopMM f g i y = monApply f $ g i y

atopMD :: FuncM -> FuncD -> FuncD -- X(f g)Y = f X g Y
atopMD f g i x y = monApply f $ g i x y

forkADD :: Array -> FuncD -> FuncD -> FuncD -- X(Z g h)Y = Z g X h Y
forkADD z g h i x y = dyadApply g (\i' -> (i', z)) $ h i x y

forkADM :: Array -> FuncD -> FuncM -> FuncM -- (X g h)Y = X g h Y
forkADM x g h i y = dyadApply g (\i' -> (i', x)) $ h i y

forkDDD :: FuncD -> FuncD -> FuncD -> FuncD -- X(f g h)Y = (X f Y) g (X h Y)
forkDDD f g h i x y = dyadApply g (\i' -> f i' x y) $ h i x y

forkMDM :: FuncM -> FuncD -> FuncM -> FuncM -- (f g h)Y = (f Y) g (h Y)
forkMDM f g h i y = dyadApply g (\i' -> f i' y) $ h i y

{- Eval Atop/Fork -}

atop :: IdMap -> FnTreeNode -> FnTreeNode -> (IdMap, Function)
atop i f g = ((i'',
    case (f', g') of
    (MonFn _ fF, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonFn _ fF, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonFn _ fF, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    (MonDyadFn _ fF _, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonDyadFn _ fF _, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    _ -> undefined -- TODO exception (internal)
    ))
    where (i', f') = evalFnTree i f
          (i'', g') = evalFnTree i' g
          dName = "der_atop"

fork :: IdMap -> FnTreeNode -> FnTreeNode -> FnTreeNode -> (IdMap, Function)
fork i f g h = case (f, g, h) of
    (_, FnLeafArr _, _) -> undefined -- TODO exception (internal)
    (_, _, FnLeafArr _) -> undefined -- TODO exception (internal)
    (FnLeafArr fA', _, _) -> let (i''', fA) = evalArrTree i'' fA' in ((i''',
        case (g', h') of
            (DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
            (DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
            (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
            (MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
            (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkADM fA gF hMF) (forkADD fA gF hDF)
            _ -> undefined
        ))
    _ -> let (i''', f') = evalFnTree i'' f in ((i''',
        case (f', g', h') of
        (DyadFn _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (DyadFn _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonFn _ fF, DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonFn _ fF, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonDyadFn _ fMF fDF, DyadFn _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
        (MonDyadFn _ fMF fDF, MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
        (MonDyadFn _ _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonDyadFn _ _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonDyadFn _ fF _, DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (MonDyadFn _ fF _, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkMDM fF gF hF)
        (DyadFn _ fF, DyadFn _ gF, MonDyadFn _ _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (DyadFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ _ hF) -> DyadFn dName (forkDDD fF gF hF)
        (MonFn _ fF, DyadFn _ gF, MonDyadFn _ hF _) -> MonFn dName (forkMDM fF gF hF)
        (MonFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ hF _) -> MonFn dName (forkMDM fF gF hF)
        ))
    where dName = "der_fork"
          (i', h') = evalFnTree i h
          (i'', g') = evalFnTree i' g

{- Eval Tree Fns -}

evalArrTree :: IdMap -> ArrTreeNode -> (IdMap, Array)
evalArrTree idm (ArrLeaf a) = (idm, a)
evalArrTree idm (ArrInternalMonFn ft at) = case evalFnTree idm ft of
    (idm', (MonFn _ f)) -> f idm' at
    (idm', (MonDyadFn _ f _)) -> f idm' at
    _ -> undefined -- TODO exception
evalArrTree idm (ArrInternalDyadFn ft at1 at2) = case evalFnTree idm ft of
    (idm', (DyadFn _ f)) -> f idm' at1 at2
    (idm', (MonDyadFn _ _ f)) -> f idm' at1 at2
    _ -> undefined -- TODO exception
evalArrTree idm (ArrInternalSubscript a is) = undefined -- TODO implement
evalArrTree idm (ArrInternalAssignment it a) = (mapInsert it (IdArr a') idm', a') -- TODO actually assign to iterator, not just id
    where (idm', a') = evalArrTree idm a

evalFnTree :: IdMap -> FnTreeNode -> (IdMap, Function)
evalFnTree idm (FnLeafFn f) = (idm, f)
evalFnTree idm (FnLeafArr _) = undefined -- TODO internal error (?)
evalFnTree idm (FnInternalMonOp op ft) = case op of
    (MonOp _ o) -> o idm ft
    (DyadOp _ _) -> undefined
evalFnTree idm (FnInternalDyadOp op ft1 ft2) = case op of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> o idm ft1 ft2
evalFnTree idm (FnInternalAtop ft1 ft2) = atop idm ft1 ft2
evalFnTree idm (FnInternalFork ft1 ft2 ft3) = fork idm ft1 ft2 ft3
