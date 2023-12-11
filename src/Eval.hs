module Eval where
import Lex
import GrammarTree
import {-# SOURCE #-} Parse

{- Helpers -}

showAndCountPad :: Show a => a -> (String, Int)
showAndCountPad x = (s, cntPad s)
    where s = show x
          cntPad (' ':cs) = 1 + cntPad cs -- counts leading spaces/ left branches
          cntPad ('┌':cs) = 1 + cntPad cs
          cntPad ('─':cs) = 1 + cntPad cs
          cntPad _ = 0

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
atop i f g = (i'',
    case (f', g') of
    (MonFn _ fF, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonFn _ fF, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonFn _ fF, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    (MonDyadFn _ fF _, MonFn _ gF) -> MonFn dName (atopMM fF gF)
    (MonDyadFn _ fF _, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
    (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
    _ -> undefined -- TODO exception (internal)
    )
    where dName = fst $ showAtopHelper (showAndCountPad f') (showAndCountPad g')
          (i', f') = evalFnTree i f
          (i'', g') = evalFnTree i' g

fork :: IdMap -> FnTreeNode -> FnTreeNode -> FnTreeNode -> (IdMap, Function)
fork i f g h = case (f, g, h) of
    (_, FnLeafArr _, _) -> undefined -- TODO exception (internal)
    (_, _, FnLeafArr _) -> undefined -- TODO exception (internal)
    (FnLeafArr fA', _, _) -> let (i''', fA) = evalArrTree i'' fA'
                                 dName = fst $ showForkHelper (showAndCountPad fA) (showAndCountPad g') (showAndCountPad h')
                             in
        (i''',
        case (g', h') of
            (DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
            (DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
            (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
            (MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
            (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkADM fA gF hMF) (forkADD fA gF hDF)
            _ -> undefined
        )
    _ -> let (i''', f') = evalFnTree i'' f
             dName = fst $ showForkHelper (showAndCountPad f') (showAndCountPad g') (showAndCountPad h')
         in
        (i''',
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
        )
    where (i', h') = evalFnTree i h
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
evalFnTree idm' orig@(FnInternalMonOp otn ft) = case uot of
    (MonOp _ o) -> o idm ft
    (DyadOp _ _) -> undefined
    where (idm, uot) = evalOpTree idm' otn
evalFnTree idm' orig@(FnInternalDyadOp otn ft1 ft2) = case uot of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> o idm ft1 ft2
    where (idm, uot) = evalOpTree idm' otn
evalFnTree idm orig@(FnInternalAtop ft1 ft2) = atop idm ft1 ft2
evalFnTree idm orig@(FnInternalFork ft1 ft2 ft3) = fork idm ft1 ft2 ft3
evalFnTree idm (FnInternalAssignment id next) = (mapInsert id (IdFn f) idm', f)
    where (idm', f) = evalFnTree idm next
evalFnTree idm (FnInternalDummyNode next) = evalFnTree idm next

evalOpTree :: IdMap -> OpTreeNode -> (IdMap, Operator)
evalOpTree idm (OpLeaf o) = (idm, o)
evalOpTree idm (OpInternalAssignment id next) = (mapInsert id (IdOp o) idm', o)
    where (idm', o) = evalOpTree idm next
evalOpTree idm (OpInternalDummyNode next) = evalOpTree idm next

{- Eval Dfns -}

mkDfn :: [Token] -> Function
mkDfn toks = MonDyadFn name (evalDfnM toks) (evalDfnD toks)
    where name = ("{" ++ (concat . map (show) $ toks) ++ "}")

evalDfnM :: [Token] -> FuncM
evalDfnM toks idm arg = (idm', snd $ execDfnStatement idm'''' toks)
    where (idm', arg') = evalArrTree idm arg
          idm'' = mapInsert "⍵" (IdArr arg') idm'
          idm''' = mapDelete "⍺" idm''
          idm'''' = mapInsert "∇" (IdTokList toks False False) idm'''

evalDfnD :: [Token] -> FuncD
evalDfnD toks idm arg1 arg2 = (idm'', snd $ execDfnStatement idm5 toks)
    where (idm', arg1') = evalArrTree idm arg1
          (idm'', arg2') = evalArrTree idm' arg2
          idm''' = mapInsert "⍺" (IdArr arg1') idm''
          idm'''' = mapInsert "⍵" (IdArr arg2') idm'''
          idm5 = mapInsert "∇" (IdTokList toks False False) idm''''

execDfnStatement :: IdMap -> [Token] -> (IdMap, Array)
execDfnStatement _ [] = undefined -- TODO expected result (or make mechanism for no result)
execDfnStatement idm toks = case parseDfnExpr (idm, toks) of
    Nothing -> undefined -- TODO syntax error
    (Just (res, toks')) -> case res of
        (DResAtn atn True) -> evalArrTree idm atn
        (DResAtn atn False) -> let (idm', _) = evalArrTree idm atn
                               in execDfnStatement idm' toks'
        (DResFtn ftn) -> let (idm', _) = evalFnTree idm ftn
                               in execDfnStatement idm' toks'
        (DResOtn otn) -> let (idm', _) = evalOpTree idm otn
                               in execDfnStatement idm' toks'
        (DResNull) -> execDfnStatement idm toks'
        (DResCond cond res) -> case evalArrTree idm cond of
            (idm', a)
                | arrIsTrue a -> evalArrTree idm' res
                | arrIsFalse a -> execDfnStatement idm' toks'
            _ -> undefined -- TODO lhs of guard must be boolean singleton
    where arrIsTrue a = a == (arrFromList [ScalarNum $ Left 1]) || (a == arrFromList [ScalarNum $ Right 1.0])
          arrIsFalse a = a == (arrFromList [ScalarNum $ Left 0]) || (a == arrFromList [ScalarNum $ Right 0.0])
