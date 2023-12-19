module Eval where
import Lex
import GrammarTree
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import {-# SOURCE #-} Parse

{- Helpers -}

countPad :: String -> (String, Int)
countPad s = (s, _countPad s)
    where _countPad (' ':cs) = 1 + _countPad cs -- counts leading spaces/ left branches
          _countPad ('┌':cs) = 1 + _countPad cs
          _countPad ('─':cs) = 1 + _countPad cs
          _countPad _ = 0

showAndCountPad :: Show a => a -> (String, Int)
showAndCountPad x = countPad . show $ x
    where s = show x

hackShowTreeM :: String -> String -> String
hackShowTreeM x headStr = fst $ showMonTreeHelper (countPad x) headStr

hackShowTreeD :: String -> String -> String -> String
hackShowTreeD x y headStr = fst $ showDyadTreeHelper (countPad x) (countPad y) headStr

expectFunc :: (Either Function Array) -> Function
expectFunc eaf = case eaf of
    (Left f) -> f
    (Right  _) -> undefined -- TODO exception: expected function

expectArr :: (Either Function Array) -> Array
expectArr eaf = case eaf of
    (Left _) -> undefined
    (Right a) -> a

liftHomoEither :: Either a a -> a
liftHomoEither eaa = case eaa of
    Left l -> l
    Right r -> r

atopMM :: FuncM -> FuncM -> FuncM -- (f g)Y = f g Y
atopMM f g y = f =<< g y

atopMD :: FuncM -> FuncD -> FuncD -- X(f g)Y = f X g Y
atopMD f g x y = f =<< g x y

forkADD :: Array -> FuncD -> FuncD -> FuncD -- X(Z g h)Y = Z g X h Y
forkADD z g h x y = g z =<< h x y

forkADM :: Array -> FuncD -> FuncM -> FuncM -- (X g h)Y = X g h Y
forkADM x g h y = g x =<< h y

forkDDD :: FuncD -> FuncD -> FuncD -> FuncD -- X(f g h)Y = (X f Y) g (X h Y)
forkDDD f g h x y = join $ g <$> (f x y) <*> (h x y)

forkMDM :: FuncM -> FuncD -> FuncM -> FuncM -- (f g h)Y = (f Y) g (h Y)
forkMDM f g h y = join $ g <$> (f y) <*> (h y)

{- Eval Atop/Fork -}

atop :: FnTreeNode -> FnTreeNode -> StateT IdMap IO Function
atop f g = do
    f' <- expectFunc <$> evalFnTree f
    g' <- expectFunc <$> evalFnTree g
    let dName = fst $ showAtopHelper (showAndCountPad f') (showAndCountPad g')
    return $ case (f', g') of
        (MonFn _ fF, MonFn _ gF) -> MonFn dName (atopMM fF gF)
        (MonFn _ fF, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
        (MonFn _ fF, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
        (MonDyadFn _ fF _, MonFn _ gF) -> MonFn dName (atopMM fF gF)
        (MonDyadFn _ fF _, DyadFn _ gF) -> DyadFn dName (atopMD fF gF)
        (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> MonDyadFn dName (atopMM fF gMF) (atopMD fF gDF)
        _ -> undefined -- TODO exception (internal)

fork :: FnTreeNode -> FnTreeNode -> FnTreeNode -> StateT IdMap IO Function
fork f g h = do
    h' <- expectFunc <$> evalFnTree h
    g' <- expectFunc <$> evalFnTree g
    _f' <- evalFnTree f
    case _f' of
        Right fA -> do
            let dName = fst $ showForkHelper (showAndCountPad fA) (showAndCountPad g') (showAndCountPad h')
            return $ case (g', h') of
                (DyadFn _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
                (DyadFn _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
                (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn dName (forkADD fA gF hF)
                (MonDyadFn _ _ gF, MonFn _ hF) -> MonFn dName (forkADM fA gF hF)
                (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn dName (forkADM fA gF hMF) (forkADD fA gF hDF)
        Left f' -> do
            let dName = fst $ showForkHelper (showAndCountPad f') (showAndCountPad g') (showAndCountPad h')
            return $ case (f', g', h') of
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

{- Eval Tree Fns -}

evalArrTree :: ArrTreeNode -> StateT IdMap IO Array
evalArrTree (ArrLeaf a) = return a
evalArrTree (ArrInternalMonFn ft at) = do
    a <- evalArrTree at
    f <- expectFunc <$> evalFnTree ft
    case f of
        MonFn _ f' -> f' a
        MonDyadFn _ f' _ -> f' a
        _ -> undefined -- TODO exception - function isn't monadic
evalArrTree (ArrInternalDyadFn ft at1 at2) = do
    a2 <- evalArrTree at2
    a1 <- evalArrTree at1
    f <- expectFunc <$> evalFnTree ft
    case f of
        DyadFn _ f' -> f' a1 a2
        MonDyadFn _ _ f' -> f' a1 a2
        _ -> undefined -- TODO exception - function isn't dyadic
evalArrTree (ArrInternalAssignment id at) = do
    a <- evalArrTree at
    idm <- get
    put $ mapInsert id (IdArr a) idm
    return a
evalArrTree (ArrInternalModAssignment id f rhs) = do
    rhs' <- evalArrTree rhs
    idm <- get
    let lhs = ArrLeaf $ case mapLookup id idm of
          Just (IdArr a) -> a
          _ -> undefined -- TODO error: undefined name
    res <- evalArrTree $ ArrInternalDyadFn f lhs (ArrLeaf rhs')
    idm' <- get
    put $ mapInsert id (IdArr res) idm'
    return rhs'
evalArrTree (ArrInternalSubscript a is) = undefined -- TODO implement
evalArrTree (ArrInternalImplCat at1 at2) = do
    a2 <- evalArrTree at2
    a1 <- evalArrTree at1
    let a2' = case at2 of
            ArrInternalMonFn (FnLeafFn fImplicitGroup) _ -> arrFromList [maybeEnclose a2]
            _ -> a2
    let a1' = case at1 of
            ArrInternalMonFn (FnLeafFn fImplicitGroup) _ -> arrFromList [maybeEnclose a1]
            _ -> a1
    return $ arrCat a1' a2'
    where maybeEnclose arr = case arrToList arr of
              (s:[]) -> s
              _ -> ScalarArr arr

evalFnTree :: FnTreeNode -> StateT IdMap IO (Either Function Array)
evalFnTree (FnLeafFn ftn) = return $ Left ftn
evalFnTree (FnLeafArr atn) = Right <$> evalArrTree atn
evalFnTree orig@(FnInternalMonOp otn ft) = evalOpTree otn >>= \x -> case x of
    (MonOp _ o) -> do
        efa <- evalFnTree ft
        Left <$> o efa
    (DyadOp _ _) -> undefined
evalFnTree orig@(FnInternalDyadOp otn ft1 ft2) = evalOpTree otn >>= \x -> case x of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> do
        efa2 <- evalFnTree ft2
        efa1 <- evalFnTree ft1
        Left <$> o efa1 efa2
evalFnTree orig@(FnInternalAtop ft1 ft2) = Left <$> atop ft1 ft2
evalFnTree orig@(FnInternalFork ft1 ft2 ft3) = Left <$> fork ft1 ft2 ft3
evalFnTree (FnInternalAssignment id next) = do
    f <- expectFunc <$> evalFnTree next
    idm <- get
    put $ mapInsert id (IdFn f) idm
    return . Left $ f
evalFnTree (FnInternalDummyNode next) = evalFnTree next

evalOpTree :: OpTreeNode -> StateT IdMap IO Operator
evalOpTree (OpLeaf o) = return o
evalOpTree (OpInternalAssignment id next) = do
    o <- evalOpTree next
    idm <- get
    put $ mapInsert id (IdOp o) idm
    return o
evalOpTree (OpInternalDummyNode next) = evalOpTree next

{- Eval Dfns -}

mkDfn :: [IdMap -> IdMap] -> [Token] -> Function
mkDfn idtfs toks = MonDyadFn (showTokListAsDfn toks) (evalDfnM idtfs toks) (evalDfnD idtfs toks)

evalDfnM :: [IdMap -> IdMap] -> [Token] -> FuncM
evalDfnM idtfs toks arg = do
    idm <- get
    put $ foldl (flip ($)) idm (idtfs ++ [
            mapInsert "⍵" (IdArr arg),
            mapDelete "⍺",
            mapInsert "∇" (IdTokList idtfs toks False False)
        ])
    execDfnStatement toks

evalDfnD :: [IdMap -> IdMap] -> [Token] -> FuncD
evalDfnD idtfs toks arg1 arg2 = do
    idm <- get
    put $ foldl (flip ($)) idm (idtfs ++ [
            mapInsert "⍺" (IdArr arg1),
            mapInsert "⍵" (IdArr arg2),
            mapInsert "∇" (IdTokList idtfs toks False False)
        ])
    execDfnStatement toks

mkDop :: [IdMap -> IdMap] -> [Token] -> Bool -> Operator -- Bool: isDyadic
mkDop idtfs toks False = MonOp (showTokListAsDfn toks) (evalDopM idtfs toks)
mkDop idtfs toks True = DyadOp (showTokListAsDfn toks) (evalDopD idtfs toks)

evalDopM :: [IdMap -> IdMap] -> [Token] -> OpM
evalDopM idtfs toks arg = do
    let argAsIdEntry = case arg of
            (Right a) -> IdArr a
            (Left f) -> IdFn f
    let idtfs' = idtfs ++ [
                mapInsert "⍺⍺" argAsIdEntry,
                mapDelete "⍵⍵",
                mapInsert "∇∇" (IdTokList [] toks True False)
            ]
    return $ MonDyadFn (showTokListAsDfn toks) (evalDfnM idtfs' toks) (evalDfnD idtfs' toks)

evalDopD :: [IdMap -> IdMap] -> [Token] -> OpD
evalDopD idtfs toks arg1 arg2 = do
    let arg1AsIdEntry =case arg1 of
            (Right a) -> IdArr a
            (Left f) -> IdFn f
    let arg2AsIdEntry = case arg2 of
            (Right a) -> IdArr a
            (Left f) -> IdFn f
    let idtfs' = idtfs ++ [
                mapInsert "⍺⍺" arg1AsIdEntry,
                mapInsert "⍵⍵" arg2AsIdEntry,
                mapInsert "∇∇" (IdTokList [] toks True True)
            ]
    return $ MonDyadFn (showTokListAsDfn toks) (evalDfnM idtfs' toks) (evalDfnD idtfs' toks)

execDfnStatement :: [Token] -> StateT IdMap IO Array
execDfnStatement [] = undefined -- TODO expected return val (or make mechanism for no value)
execDfnStatement toks = do
    idm <- get
    case evalMatchFn idm toks parseDfnExpr of
        Nothing -> undefined -- TODO syntax error
        (Just (res, toks')) -> case res of
            (DResAtn atn True) -> evalArrTree atn
            (DResAtn atn False) -> evalArrTree atn *> execDfnStatement toks'
            (DResFtn ftn) -> evalFnTree ftn *> execDfnStatement toks'
            (DResOtn otn) -> evalOpTree otn *> execDfnStatement toks'
            (DResNull) -> execDfnStatement toks'
            (DResDefaultAlpha atn) -> case mapLookup "⍺" idm of
                (Just _) -> execDfnStatement toks'
                Nothing -> evalArrTree atn >>= \a -> (put $ mapInsert "⍺" (IdArr a) idm) *> execDfnStatement toks'
            (DResCond cond res) -> evalArrTree cond >>= \x -> case x of
                a
                    | a == arrFromList [ScalarNum 1.0] -> evalArrTree res
                    | a == arrFromList [ScalarNum 0.0] -> execDfnStatement toks'
                _ -> undefined -- TODO lhs of guard must be boolean singleton
