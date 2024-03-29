module Eval where
import Lex
import GrammarTree
import PrettyPrint
import Control.Monad (mapM)
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
import Util
import {-# SOURCE #-} qualified Functions as F
import Data.Function

{- Helpers -}

infoToNamePad :: FnInfoT t => t -> (String, Int)
infoToNamePad = liftA2 (,) fnInfoName fnInfoNamePad

class ShowAndPad a where
    showAndPad :: a -> (String, Int)

instance ShowAndPad Function where
    showAndPad (MonFn i _) = infoToNamePad i
    showAndPad (DyadFn i _) = infoToNamePad i
    showAndPad (AmbivFn i _ _) = infoToNamePad i

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

autoInfoMonFnM :: ShowAndPad a => String -> a -> FuncM -> Function
autoInfoMonFnM s a = MonFn (namePadToFnInfoM $ showMonTreeHelper (showAndPad a) s)

autoInfoDyadFnM :: ShowAndPad a => String -> a -> FuncD -> Function
autoInfoDyadFnM s a = DyadFn (namePadToFnInfoD $ showMonTreeHelper (showAndPad a) s)

autoInfoAmbivFnM :: ShowAndPad a => String -> a -> FuncM -> FuncD -> Function
autoInfoAmbivFnM s a = AmbivFn (namePadToFnInfoA $ showMonTreeHelper (showAndPad a) s)

autoInfoMonFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncM -> Function
autoInfoMonFnD s a b = MonFn (namePadToFnInfoM $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

autoInfoDyadFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncD -> Function
autoInfoDyadFnD s a b = DyadFn (namePadToFnInfoD $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

autoInfoAmbivFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncM -> FuncD -> Function
autoInfoAmbivFnD s a b = AmbivFn (namePadToFnInfoA $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

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
        (MonFn _ fF, AmbivFn _ gMF gDF) -> AmbivFn infoA (atopMM fF gMF) (atopMD fF gDF)
        (AmbivFn _ fF _, MonFn _ gF) -> MonFn infoM (atopMM fF gF)
        (AmbivFn _ fF _, DyadFn _ gF) -> DyadFn infoD (atopMD fF gF)
        (AmbivFn _ fF _, AmbivFn _ gMF gDF) -> AmbivFn infoA (atopMM fF gMF) (atopMD fF gDF)
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
                (AmbivFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkADD fA gF hF)
                (AmbivFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkADM fA gF hF)
                (AmbivFn _ _ gF, AmbivFn _ hMF hDF) -> AmbivFn infoA (forkADM fA gF hMF) (forkADD fA gF hDF)
        Right f' -> do
            let (infoM, infoD, infoA) = namePadToFnInfoMDA $ showForkHelper (showAndPad f') (showAndPad g') (showAndPad h')
            return $ case (f', g', h') of
                (DyadFn _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (DyadFn _ fF, AmbivFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonFn _ fF, DyadFn _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (MonFn _ fF, AmbivFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (AmbivFn _ fMF fDF, DyadFn _ gF, AmbivFn _ hMF hDF) -> AmbivFn infoA (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
                (AmbivFn _ fMF fDF, AmbivFn _ _ gF, AmbivFn _ hMF hDF) -> AmbivFn infoA (forkMDM fMF gF hMF) (forkDDD fDF gF hDF)
                (AmbivFn _ _ fF, DyadFn _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (AmbivFn _ _ fF, AmbivFn _ _ gF, DyadFn _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (AmbivFn _ fF _, DyadFn _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (AmbivFn _ fF _, AmbivFn _ _ gF, MonFn _ hF) -> MonFn infoM (forkMDM fF gF hF)
                (DyadFn _ fF, DyadFn _ gF, AmbivFn _ _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (DyadFn _ fF, AmbivFn _ _ gF, AmbivFn _ _ hF) -> DyadFn infoD (forkDDD fF gF hF)
                (MonFn _ fF, DyadFn _ gF, AmbivFn _ hF _) -> MonFn infoM (forkMDM fF gF hF)
                (MonFn _ fF, AmbivFn _ _ gF, AmbivFn _ hF _) -> MonFn infoM (forkMDM fF gF hF)

{- Subscripting -}

evalArrSubscript :: Int -> Array -> [Maybe Array] -> Array
evalArrSubscript _ lhs ixs
    | ((arrRank lhs) /= (length ixs)) && (length ixs /= 1 || ixs == [Nothing]) = throw . RankError $ "([]): wrong size of index list"
evalArrSubscript iO lhs [Just arr]
    | otherwise = arrMap elemAt arr
    where elemAt s = case s of
              ScalarNum n
                  | isIntegral n && validIndex [n'] -> lhs `arrIndex` [n']
                    where n' = (floor n) - iO
              ScalarArr a
                  | all isScalarInt (arrToList a) && validIndex (map (+(-1*iO)) $ arrToIntVec a) -> lhs `arrIndex` map (+(-1*iO)) (arrToIntVec a)
              _ -> throw . RankError $ "invalid index: " ++ (show s)
          validIndex ns = (length ns == _rank) && (all (>=0) ns) && (all id (zipWith (<) ns _shape))
          _rank = arrRank lhs
          _shape = shape lhs
          isScalarInt s =  case s of
              ScalarNum n -> isIntegral n
              _ -> False
evalArrSubscript iO lhs ixs = partialFlatten $ shapedArrFromList shape' cells'
    where shape' = map length $ intVecs
          cells' = if all id $ zipWith (\x iv -> all (>=0) iv && all (<x) iv) (shape lhs) intVecs
                   then map (lhs`arrIndex`) . sequence $ intVecs -- sequence is n-wise cartesian product for lists
                   else throw $ LengthError "index out of range"
          intVecs = zipWith toIv ixs (shape lhs)
          toIv mbA i = case mbA of
                   Nothing -> [0..(i - 1)]
                   Just a -> map (+(-1*iO)) $ arrToIntVec a

{- Selective Assignment -}

evalArrSelAss :: ArrTreeNode -> ArrTreeNode -> StateT IdMap IO Array
evalArrSelAss lhs rhs = case lhs of
    ArrInternalImplGroup lhs'
        | ArrInternalImplGroup rhs' <- rhs -> evalArrSelAss lhs' rhs'
        | otherwise -> evalArrSelAss lhs' rhs
    ArrInternalImplCat ll lr
        | ArrInternalImplCat rl rr <- rhs -> join $ evalArrTree .: (on ArrInternalImplCat ArrLeaf) <$> evalArrSelAss ll rl <*> evalArrSelAss lr rr
        | otherwise -> do rhs' <- evalArrTree rhs
                          if shape rhs' == [0] || arrRank rhs' /= 1 then throw . SyntaxError $ "bad selective assignment (lhs is implicit cat, rhs is not cons)"
                          else let rl = ArrLeaf . scalarToArr . head . arrToList $ rhs'
                                   rr = ArrLeaf . listToArr_ . tail . arrToList $ rhs'
                               in join $ evalArrTree .: (on ArrInternalImplCat ArrLeaf) <$> evalArrSelAss ll rl <*> evalArrSelAss lr rr

    ArrLeafVar id -> evalArrTree $ ArrInternalAssignment id rhs
    _ -> do
        id <- getSelectedVar lhs
        idm <- get
        let varArr = case mapLookup id idm of
                Nothing -> throw . NameError $ "(←): undefined name: `" ++ id ++ "`"
                Just (IdArr a) -> a
                Just _ -> throw . NameError $ "(←): invalid class (expected array): `" ++ id ++ "`"
        rhsEvaluated <- evalArrTree rhs
        iotaShapeVar <- F.toEvalM . F.iota $ F.shapeOf varArr
        lhs' <- evalArrTree $ arrTreeSubVar lhs id iotaShapeVar
        if shape lhs' /= shape rhsEvaluated then throw . SyntaxError $ "(←): mismatched shapes"
        else do iO <- arrToInt <$> getQIo
                let varArr' = arrModL varArr $ zipWith (,) (map (map (+(-1*iO)) . arrToIntVec . scalarToArr) . arrToList $ lhs') (arrToList rhsEvaluated)
                put $ mapInsert id (IdArr varArr') idm
                return rhsEvaluated
    where getSelectedVar :: ArrTreeNode -> StateT IdMap IO String -- also asserts that the argument is selectable
          getSelectedVar (ArrLeafVar id) = return id
          getSelectedVar (ArrInternalSubscript atn _) = getSelectedVar atn
          getSelectedVar (ArrInternalMonFn ftn atn) = do f <- expectFunc <$> evalFnTree ftn
                                                         if fnCanSelect (fnCanSelectAM) f
                                                         then getSelectedVar atn
                                                         else throw . SyntaxError $ "(←): internal function can't select: (\n" ++ (show ftn) ++ "\n)"
          getSelectedVar (ArrInternalDyadFn ftn _ atn) = do f <- expectFunc <$> evalFnTree ftn
                                                            if fnCanSelect (fnCanSelectAD) f
                                                            then getSelectedVar atn
                                                            else throw . SyntaxError $ "(←): internal function can't select: (\n" ++ (show ftn) ++ "\n)"
          getSelectedVar (ArrInternalImplGroup atn) = getSelectedVar atn
          getSelectedVar _ = throw . SyntaxError $ "(←): bad selective assignment: not homogeneous (non-variable array)"
          arrTreeSubVar :: ArrTreeNode -> String -> Array -> ArrTreeNode
          arrTreeSubVar (ArrLeafVar s) id sub
              | s == id = ArrLeaf sub
              | otherwise = undefined
          arrTreeSubVar (ArrInternalSubscript atn ixs) id sub = ArrInternalSubscript (arrTreeSubVar atn id sub) ixs
          arrTreeSubVar (ArrInternalMonFn fn atn) id sub = ArrInternalMonFn fn (arrTreeSubVar atn id sub)
          arrTreeSubVar (ArrInternalDyadFn fn atn1 atn2) id sub = ArrInternalDyadFn fn atn1 (arrTreeSubVar atn2 id sub)
          arrTreeSubVar (ArrInternalImplGroup atn) id sub = ArrInternalImplGroup (arrTreeSubVar atn id sub)
          arrTreeSubVar x _ _ = throw . SyntaxError $ "err: " ++ (show x) -- TODO remove
          fnCanSelect ambivSelector fn = case fn of
              MonFn i _ -> fnCanSelectM i
              DyadFn i _ -> fnCanSelectD i
              AmbivFn i _ _ -> ambivSelector i

{- Axis Spec -}

evalAxisSpec :: Function -> Array -> Function
evalAxisSpec f a = case f of
    (MonFn i _) -> case fnOnAxisM i of
        Nothing -> throw . SyntaxError $ "([]): function doesn't implement axis specification"
        Just fAx -> autoInfoMonFnD "[]" f a (fAx ax)
    (DyadFn i _) -> case fnOnAxisD i of
        Nothing -> throw . SyntaxError $ "([]): function doesn't implement axis specification"
        Just fAx -> autoInfoDyadFnD "[]" f a (fAx ax)
    (AmbivFn i _ _) -> case (fnOnAxisAM i, fnOnAxisAD i) of
        (Nothing, Nothing) -> throw . SyntaxError $ "([]): function doesn't implement axis specification"
        (Just fM, Nothing) -> autoInfoMonFnD "[]" f a (fM ax)
        (Nothing, Just fD) -> autoInfoDyadFnD "[]" f a (fD ax)
        (Just fM, Just fD) -> autoInfoAmbivFnD "[]" f a (fM ax) (fD ax)
    where unwrapScalarNum a
              | shape a /= [1] = throw . RankError $ "([]): invalid axis"
              | ScalarNum n <- a `at` 0 = n
              | otherwise = throw . RankError $ "([]): invalid axis"
          ax = unwrapScalarNum a

{- Eval Tree Fns -}

evalArrTree :: ArrTreeNode -> StateT IdMap IO Array
evalArrTree (ArrLeaf a) = return a
evalArrTree (ArrLeafVar id) = do
    idm <- get
    case mapLookup id idm of
        Nothing -> throw . NameError $ "undefined name: `" ++ id ++ "`"
        Just (IdArr a) -> return a
        Just _ -> throw . NameError $ "invalid class (expected array): `" ++ id ++ "`"
evalArrTree (ArrInternalMonFn ft at) = do
    a <- evalArrTree at
    f <- expectFunc <$> evalFnTree ft
    case f of
        MonFn _ f' -> f' a
        AmbivFn _ f' _ -> f' a
        _ -> throw . SyntaxError $ "expected monadic function: (\n" ++ (show f) ++ "\n)"
evalArrTree (ArrNiladicFn _ f) = f
evalArrTree (ArrInternalDyadFn ft at1 at2) = do
    a2 <- evalArrTree at2
    a1 <- evalArrTree at1
    f <- expectFunc <$> evalFnTree ft
    case f of
        DyadFn _ f' -> f' a1 a2
        AmbivFn _ _ f' -> f' a1 a2
        _ -> throw . SyntaxError $ "expected dyadic function: (\n" ++ (show f) ++ "\n)"
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
evalArrTree (ArrInternalSelAssignment lhs rhs) = evalArrSelAss lhs rhs
evalArrTree (ArrInternalQuadAssignment atn) = do
    a <- evalArrTree atn
    lift $ putStrLn $ show a
    return a
evalArrTree (ArrInternalQuadIdAssignment id atn) = do
    a <- evalArrTree atn
    (qset $ getQuadName id) a
evalArrTree (ArrInternalSubscript at its) = do
    a <- evalArrTree at
    is <- mapM (sequence . (evalArrTree<$>)) its
    iO <- arrToInt <$> getQIo
    return $ evalArrSubscript iO a is
evalArrTree (ArrInternalImplCat at1 at2) = do
    a2 <- evalArrTree at2
    a1 <- evalArrTree at1
    let a2' = case at2 of
            ArrInternalImplGroup _ -> listToArr [maybeEnclose a2]
            _ -> a2
    let a1' = case at1 of
            ArrInternalImplGroup _ -> listToArr [maybeEnclose a1]
            _ -> a1
    return $ arrCat a1' a2'
    where maybeEnclose arr = case arrToList arr of
              (s:[]) -> s
              _ -> ScalarArr arr
evalArrTree (ArrInternalImplGroup atn) = evalArrTree atn

evalFnTree :: FnTreeNode -> StateT IdMap IO (Either Array Function)
evalFnTree (FnLeafFn ftn) = return $ Right ftn
evalFnTree (FnLeafArr atn) = Left <$> evalArrTree atn
evalFnTree (FnLeafVar id) = do
    idm <- get
    case mapLookup id idm of
        Nothing -> throw . NameError $ "undefined name: `" ++ id ++ "`"
        Just (IdFn f) -> return . Right $ f
        Just (IdDfn toks) -> return . Right $ mkDfn toks Nothing Nothing Nothing
        Just (IdDerDfn toks aa ww dd) -> return . Right $ mkDfn toks aa ww dd
        Just _ -> throw . NameError $ "invalid class (expected function): `" ++ id ++ "`"
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
evalFnTree (FnInternalAxisSpec ftn atn) = do
    a <- evalArrTree atn
    efa <- evalFnTree ftn
    case efa of
        Left _ -> undefined -- (internal error: axis spec bound more strongly than subscript)
        Right f -> return . Right $ evalAxisSpec f a
evalFnTree (FnInternalDummyNode next) = evalFnTree next

evalOpTree :: OpTreeNode -> StateT IdMap IO Operator
evalOpTree (OpLeaf o) = return o
evalOpTree (OpLeafVar id) = do
    idm <- get
    case mapLookup id idm of
        Nothing -> throw . NameError $ "undefined name: `" ++ id ++ "`"
        Just (IdOp o) -> return o
        Just (IdDop toks is_dy) -> return $ mkDop toks is_dy
        Just _ -> throw . NameError $ "invalid class (expected operator): `" ++ id ++ "`"
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
mkDfn toks aa ww dd = AmbivFn (namePadToFnInfoA namePad) (evalDfnM toks aa ww dd) (evalDfnD toks aa ww dd)
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
                    | a == listToArr [ScalarNum 1.0] -> evalArrTree res
                    | a == listToArr [ScalarNum 0.0] -> execDfnStatement toks'
                _ -> throw $ DomainError "expected boolean singleton as lhs of gaurd"
