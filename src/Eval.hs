module Eval where
import Lex
import GrammarTree
import PrettyPrint
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor (bimap)
import Data.Maybe (isJust, fromJust)
import {-# SOURCE #-} Parse
import QuadNames
import Control.Applicative (liftA2)
import Exceptions
import Control.Exception

{- Helpers -}

infoToNamePad :: FnInfoT t => t -> (String, Int)
infoToNamePad = liftA2 (,) fnInfoName fnInfoNamePad

class ShowAndPad a where
    showAndPad :: a -> (String, Int)

instance ShowAndPad Function where
    showAndPad (MonFn i _) = infoToNamePad i
    showAndPad (DyadFn i _) = infoToNamePad i
    showAndPad (MonDyadFn i _ _) = infoToNamePad i

instance ShowAndPad Array where
    showAndPad a = (show a, 0)

instance (ShowAndPad a, ShowAndPad b) => ShowAndPad (Either a b) where
    showAndPad = fromHomoEither . bimap showAndPad showAndPad

namePadToFnInfoM :: (String, Int) -> FnInfoM
namePadToFnInfoM (name, pad) = defFnInfoM {fnNameM = name, fnNamePadM = pad}

namePadToFnInfoD :: (String, Int) -> FnInfoD
namePadToFnInfoD (name, pad) = defFnInfoD {fnNameD = name, fnNamePadD = pad}

namePadToFnInfoA :: (String, Int) -> FnInfoA
namePadToFnInfoA (name, pad) = defFnInfoA {fnNameA = name, fnNamePadA = pad}

namePadToFnInfoMDA :: (String, Int) -> (FnInfoM, FnInfoD, FnInfoA)
namePadToFnInfoMDA (name, pad) = (defFnInfoM {fnNameM = name, fnNamePadM = pad},
                                  defFnInfoD {fnNameD = name, fnNamePadD = pad},
                                  defFnInfoA {fnNameA = name, fnNamePadA = pad})

expectFunc :: (Either Array Function) -> Function
expectFunc eaf = case eaf of
    (Left  _) -> undefined
    (Right f) -> f

expectArr :: (Either Array Function) -> Array
expectArr eaf = case eaf of
    (Left a) -> a
    (Right _) -> undefined

fromHomoEither :: Either a a -> a
fromHomoEither eaa = case eaa of
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
    let (infoM, infoD, infoA) = namePadToFnInfoMDA $ showAtopHelper (showAndPad f') (showAndPad g')
    return $ case (f', g') of
        (MonFn _ fF, MonFn _ gF) -> MonFn infoM (atopMM fF gF)
        (MonFn _ fF, DyadFn _ gF) -> DyadFn infoD (atopMD fF gF)
        (MonFn _ fF, MonDyadFn _ gMF gDF) -> MonDyadFn infoA (atopMM fF gMF) (atopMD fF gDF)
        (MonDyadFn _ fF _, MonFn _ gF) -> MonFn infoM (atopMM fF gF)
        (MonDyadFn _ fF _, DyadFn _ gF) -> DyadFn infoD (atopMD fF gF)
        (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> MonDyadFn infoA (atopMM fF gMF) (atopMD fF gDF)
        _ -> undefined

fork :: FnTreeNode -> FnTreeNode -> FnTreeNode -> StateT IdMap IO Function
fork f g h = do
    h' <- expectFunc <$> evalFnTree h
    g' <- expectFunc <$> evalFnTree g
    _f' <- evalFnTree f
    case _f' of
        Left fA -> do
            let (infoM, infoD, infoA) = namePadToFnInfoMDA $ showForkHelper (show fA, 0) (showAndPad g') (showAndPad h')
            return $ case (g', h') of
                (DyadFn _ gF, DyadFn _ hF) -> DyadFn infoD (forkADD fA gF hF)
                (DyadFn _ gF, MonFn _ hF) -> MonFn infoM (forkADM fA gF hF)
                (MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkADD fA gF hF)
                (MonDyadFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkADM fA gF hF)
                (MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn infoA (forkADM fA gF hMF) (forkADD fA gF hDF)
        Right f' -> do
            let (infoM, infoD, infoA) = namePadToFnInfoMDA $ showForkHelper (showAndPad f') (showAndPad g') (showAndPad h')
            return $ case (f', g', h') of
                (DyadFn _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (DyadFn _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonFn _ fF, DyadFn _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (MonFn _ fF, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (MonDyadFn _ fMF fDF, DyadFn _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn infoA (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
                (MonDyadFn _ fMF fDF, MonDyadFn _ _ gF, MonDyadFn _ hMF hDF) -> MonDyadFn infoA (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
                (MonDyadFn _ _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonDyadFn _ _ fF, MonDyadFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonDyadFn _ fF _, DyadFn _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (MonDyadFn _ fF _, MonDyadFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (DyadFn _ fF, DyadFn _ gF, MonDyadFn _ _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (DyadFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonFn _ fF, DyadFn _ gF, MonDyadFn _ hF _) -> MonFn infoM (forkMDM fF gF hF)
                (MonFn _ fF, MonDyadFn _ _ gF, MonDyadFn _ hF _) -> MonFn infoM (forkMDM fF gF hF)

{- Eval Tree Fns -}

evalArrTree :: ArrTreeNode -> StateT IdMap IO Array
evalArrTree (ArrLeaf a) = return a
evalArrTree (ArrInternalMonFn ft at) = do
    a <- evalArrTree at
    f <- expectFunc <$> evalFnTree ft
    case f of
        MonFn _ f' -> f' a
        MonDyadFn _ f' _ -> f' a
        _ -> undefined -- function should be monadic
evalArrTree (ArrNiladicFn _ f) = f
evalArrTree (ArrInternalDyadFn ft at1 at2) = do
    a2 <- evalArrTree at2
    a1 <- evalArrTree at1
    f <- expectFunc <$> evalFnTree ft
    case f of
        DyadFn _ f' -> f' a1 a2
        MonDyadFn _ _ f' -> f' a1 a2
        _ -> undefined -- function should be dyadic
evalArrTree (ArrInternalAssignment id at) = do
    a <- evalArrTree at
    idm <- get
    put $! mapInsert id (IdArr $! a) idm
    return a
evalArrTree (ArrInternalModAssignment id f rhs) = do
    rhs' <- evalArrTree rhs
    idm <- get
    let lhs = ArrLeaf $ case mapLookup id idm of
          Just (IdArr a) -> a
          _ -> throw . NameError $ "undefined name: `" ++ id ++ "`"
    res <- evalArrTree $ ArrInternalDyadFn f lhs (ArrLeaf rhs')
    idm' <- get
    put $! mapInsert id (IdArr $! res) idm'
    return rhs'
evalArrTree (ArrInternalQuadAssignment atn) = do
    a <- evalArrTree atn
    lift $ putStrLn $ show a
    return a
evalArrTree (ArrInternalQuadIdAssignment id atn) = do
    a <- evalArrTree atn
    (qset $ getQuadName id) a
evalArrTree (ArrInternalSubscript a is) = throw $ WipError "array subscripting"
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

evalFnTree :: FnTreeNode -> StateT IdMap IO (Either Array Function)
evalFnTree (FnLeafFn ftn) = return $ Right ftn
evalFnTree (FnLeafArr atn) = Left <$> evalArrTree atn
evalFnTree orig@(FnInternalMonOp otn ft) = evalOpTree otn >>= \x -> case x of
    (MonOp _ o) -> do
        efa <- evalFnTree ft
        Right <$> o efa
    (DyadOp _ _) -> undefined
evalFnTree orig@(FnInternalDyadOp otn ft1 ft2) = evalOpTree otn >>= \x -> case x of
    (MonOp _ _) -> undefined
    (DyadOp _ o) -> do
        efa2 <- evalFnTree ft2
        efa1 <- evalFnTree ft1
        Right <$> o efa1 efa2
evalFnTree orig@(FnInternalAtop ft1 ft2) = Right <$> atop ft1 ft2
evalFnTree orig@(FnInternalFork ft1 ft2 ft3) = Right <$> fork ft1 ft2 ft3
evalFnTree (FnInternalAssignment id next) = do
    f <- expectFunc <$> evalFnTree next
    idm <- get
    put $ mapInsert id (IdFn f) idm
    return . Right $ f
evalFnTree (FnInternalQuadAssignment next) = do
    efa <- evalFnTree next
    lift $ putStrLn . fromHomoEither . bimap show show $ efa
    return efa
evalFnTree (FnInternalDummyNode next) = evalFnTree next

evalOpTree :: OpTreeNode -> StateT IdMap IO Operator
evalOpTree (OpLeaf o) = return o
evalOpTree (OpInternalAssignment id next) = do
    o <- evalOpTree next
    idm <- get
    put $ mapInsert id (IdOp o) idm
    return o
evalOpTree (OpInternalQuadAssignment next) = do
    o <- evalOpTree next
    lift $ putStrLn . show $ o
    return o
evalOpTree (OpInternalDummyNode next) = evalOpTree next

{- Eval Dfns -}

mkDfn :: [Token] -> (Maybe IdEntry) -> (Maybe IdEntry) -> (Maybe IdEntry) -> Function
mkDfn toks aa ww dd = MonDyadFn (namePadToFnInfoA namePad) (evalDfnM toks aa ww dd) (evalDfnD toks aa ww dd)
    where namePad = case (aa, ww) of
                        (Nothing, Nothing) -> (rootStr, 0)
                        (Just ide, Nothing) -> showMonTreeHelper (_showIde ide) rootStr
                        (Just ide1, Just ide2) -> showDyadTreeHelper (_showIde ide1) (_showIde ide2) rootStr
          rootStr = showTokListAsDfn toks
          _showIde ide = case ide of
              (IdArr a) -> (show a, 0)
              (IdFn f) -> showAndPad f
              _ -> undefined

evalDfnM :: [Token] -> (Maybe IdEntry) -> (Maybe IdEntry) -> (Maybe IdEntry) -> FuncM
evalDfnM toks aa ww dd arg = do
    idm <- get
    put $ foldl (flip ($)) idm [
            (if isJust aa then mapInsert "⍺⍺" (fromJust aa) else mapDelete "⍺⍺"),
            (if isJust ww then mapInsert "⍵⍵" (fromJust ww) else mapDelete "⍵⍵"),
            (if isJust dd then mapInsert "∇∇" (fromJust dd) else mapDelete "∇∇"),
            mapInsert "⍵" (IdArr arg),
            mapDelete "⍺",
            mapInsert "∇" (IdDerDfn toks aa ww dd)
        ]
    res <- execDfnStatement toks
    put idm
    return res

evalDfnD :: [Token] -> (Maybe IdEntry) -> (Maybe IdEntry) -> (Maybe IdEntry) -> FuncD
evalDfnD toks aa ww dd arg1 arg2 = do
    idm <- get
    put $ foldl (flip ($)) idm [
            (if isJust aa then mapInsert "⍺⍺" (fromJust aa) else mapDelete "⍺⍺"),
            (if isJust ww then mapInsert "⍵⍵" (fromJust ww) else mapDelete "⍵⍵"),
            (if isJust dd then mapInsert "∇∇" (fromJust dd) else mapDelete "∇∇"),
            mapInsert "⍺" (IdArr arg1),
            mapInsert "⍵" (IdArr arg2),
            mapInsert "∇" (IdDfn toks)
        ]
    res <- execDfnStatement toks
    put idm
    return res

mkDop :: [Token] -> Bool -> Operator -- Bool: isDyadic
mkDop toks False = MonOp (showTokListAsDfn toks) (evalDopM toks)
mkDop toks True = DyadOp (showTokListAsDfn toks) (evalDopD toks)

evalDopM :: [Token] -> OpM
evalDopM toks arg = do
    idm <- get
    let aa = Just $ fromHomoEither . bimap IdArr IdFn $ arg
    let ww = Nothing
    let dd = Just $ IdDop toks False
    return $ mkDfn toks aa ww dd

evalDopD :: [Token] -> OpD
evalDopD toks arg1 arg2 = do
    let aa = Just $ fromHomoEither . bimap IdArr IdFn $ arg1
    let ww = Just $ fromHomoEither . bimap IdArr IdFn $ arg2
    let dd = Just $ IdDop toks True
    return $ mkDfn toks aa ww dd

execDfnStatement :: [Token] -> StateT IdMap IO Array
execDfnStatement [] = throw $ SyntaxError "expected return value from dfn/dop"
execDfnStatement toks = do
    idm <- get
    case evalMatchFn idm toks parseDfnExpr of
        Nothing -> throw $ SyntaxError "parse error in dfn/dop"
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
                _ -> throw $ DomainError "expected boolean singleton as lhs of gaurd"
