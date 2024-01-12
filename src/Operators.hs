module Operators where
import Eval
import GrammarTree
import PrettyPrint
import qualified Functions as F
import Control.Exception (throw)
import Exceptions
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Function
import Util
import Data.Maybe
import Data.Foldable (foldrM)
import QuadNames
import Data.List

{- Helpers -}

getDyadFn :: Function -> FuncD
getDyadFn f = case f of
   (DyadFn _ x) -> x
   (AmbivFn _ _ x) -> x
   _ -> throw $ SyntaxError "expected dyadic function"

getMonFn :: Function -> FuncM
getMonFn f = case f of
    (MonFn _ x) -> x
    (AmbivFn _ x _) -> x
    _ -> throw $ SyntaxError "expected monadic function"

getInverseM :: Function -> FuncM
getInverseM f = case f of
    MonFn i _ -> fromMaybe err (fnInverseM i)
    AmbivFn i _ _ -> fromMaybe err (fnInverseAM i)
    where err = throw . DomainError $ "function has no inverse: {" ++ (show f) ++ "}"

getInverseD :: Function -> FuncD
getInverseD f = case f of
    DyadFn i _ -> fromMaybe err (fnInverseD i)
    AmbivFn i _ _ -> fromMaybe err (fnInverseAD i)
    where err = throw . DomainError $ "function has no inverse: {" ++ (show f) ++ "}"

{- Axis-Spec Operators -}

reduce :: Bool -> Function -> F.IdxOriginM Function
reduce isFirst f = do iO <- ask
                      let _ax arg = if isFirst then fromIntegral iO else fromIntegral $ arrRank arg - 1 + iO
                      return $ AmbivFn dinfo (\y -> _reduce (_ax y) (intToScalarArr $ (shape y !! ((_ax y) - iO))) y) (\x y -> _reduce (_ax y) x y)
    where dinfo = (namePadToFnInfoA $ showMonTreeHelper (showAndPad f) "/") { fnOnAxisAM = Just _reduceM
                                                                            , fnOnAxisAD = Just _reduce }
          fd = case f of
              MonFn _ _ -> throw . DomainError $ "(/): expected dyadic function"
              DyadFn _ d -> d
              AmbivFn _ _ d -> d
          _reduceM ax y
              | not $ isIntegral ax = throw . RankError $ "(/): invalid (fractional) axis"
              | otherwise = do iO <- arrToInt <$> getQIo
                               let ax' = (floor ax) - iO + 1
                               if ax' < 1 || ax' > arrRank y then throw . RankError $ "(/): invalid axis"
                               else _reduce ax (intToScalarArr . max 0 $ shape y !! (ax' - 1)) y
          _reduce :: Double -> Array -> Array -> StateT IdMap IO Array
          _reduce ax x y
              | not $ isIntegral ax = throw . RankError $ "(/): invalid (fractional) axis"
              | y == zilde && win == 0 = return $ scalarToArr idElem
              | win <= 0 = throw . DomainError $ "(/): left argument should be nonnegative"
              | otherwise = do iO <- arrToInt <$> getQIo
                               let ax' = (floor ax) - iO + 1
                               if ax' < 1 || ax' > arrRank y then throw . RankError $ "(/): invalid axis"
                               else if (shape y !! (ax' - 1)) < win then throw . LengthError $ "(/): window too big"
                               else (fmap (maybeUnAlongAxis ax')) . mapM (mapVecsAlongAxisM ax' __reduce . unAlongAxis ax') . windowsOf win . alongAxis ax' $ y
              where win = arrToInt x
                    idElem = case f of
                        AmbivFn i _ _ -> fromMaybe noIdErr (fnIdAD i)
                        DyadFn i _ -> fromMaybe noIdErr (fnIdD i)
                    noIdErr = throw . DomainError $ "(/): no identity element for function"
                    __reduce :: [Scalar] -> StateT IdMap IO [Scalar]
                    __reduce ss = (:[]) <$> foldrM ((fmap arrToScalar) .: on fd scalarToArr) (last ss) (init ss)
                    maybeUnAlongAxis ax' [] = zilde
                    maybeUnAlongAxis ax' (a:[]) = a
                    maybeUnAlongAxis ax' as = unAlongAxis ax' as

scan :: Bool -> Function -> F.IdxOriginM Function
scan isFirst f = do iO <- ask
                    let _ax arg = if isFirst then fromIntegral iO else fromIntegral $ arrRank arg - 1 + iO
                    return $ MonFn dinfo (\y -> _scan (_ax y) y)
    where dinfo = (namePadToFnInfoM $ showMonTreeHelper (showAndPad f) "\\") {fnOnAxisM = Just _scan}
          fd = case f of
              MonFn _ _ -> throw . DomainError $ "(\\): expected dyadic function"
              DyadFn _ d -> d
              AmbivFn _ _ d -> d
          _scan :: Double -> Array -> StateT IdMap IO Array
          _scan ax y
              | y == zilde = return zilde
              | not $ isIntegral ax = throw . RankError $ "(\\): invalid (fractional) axis"
              | otherwise = do iO <- arrToInt <$> getQIo
                               let ax' = (Prelude.floor ax) - iO + 1
                               if ax' < 1 || ax' > arrRank y then throw . RankError $ "(\\): invalid axis"
                               else mapVecsAlongAxisM ax' __scan $ y
              where __scan :: [Scalar] -> StateT IdMap IO [Scalar]
                    __scan ss = mapM (_reduce) . tail . inits $ ss
                    _reduce ss = foldrM ((fmap arrToScalar) .: on fd scalarToArr) (last ss) (init ss)

{- First/Last -Axis Operators -}

reduceFirst :: Function -> F.IdxOriginM Function
reduceFirst = reduce True

reduceLast :: Function -> F.IdxOriginM Function
reduceLast = reduce False

scanFirst :: Function -> F.IdxOriginM Function
scanFirst = scan True

scanLast :: Function -> F.IdxOriginM Function
scanLast = scan False

{- General Operators -}

each :: Function -> Function
each f = case f of
    MonFn _ fM -> autoInfoMonFnM "¨" f (_eachM fM)
    DyadFn _ fD -> autoInfoDyadFnM "¨" f (_eachD fD)
    AmbivFn _ fM fD -> autoInfoAmbivFnM "¨" f (_eachM fM) (_eachD fD)
    where _eachM :: FuncM -> FuncM
          _eachM m x = arrMapM ((arrToScalar<$>) . m . scalarToArr) $ x
          _eachD :: FuncD -> FuncD
          _eachD d x y = arrZipWithM ((arrToScalar<$>) .: on d scalarToArr) x' y'
              where (x', y') = rankMorph (x, y)

innerProduct :: Function -> Function -> Function
innerProduct f g = autoInfoDyadFnD "." f g _innerProduct
    where _innerProduct :: FuncD
          _innerProduct x y
              | x == zilde && y == zilde = return . scalarToArr $ idElem
              | (x == zilde && shape y == [1]) || (shape x == [1] && y == zilde) = return . scalarToArr $ idElem
              | otherwise = shapedArrFromList shape' . arrToList <$> mapVecsAlongAxisM 1 (zipEach x') y'
              where (x', y') = extend (x, y)
                    shape' = if arrRank x' == 1 && arrRank y' == 1 then [1]
                             else (init $ shape x') ++ (tail $ shape y')
          extend (x, y)
              | shape x == [1] = (listToArr $ replicate (head $ shape y) (x `GrammarTree.at` 0), y)
              | shape y == [1] = (x, listToArr $ replicate (last $ shape x) (y `GrammarTree.at` 0))
              | (last $ shape x) /= (head $ shape y) = throw . LengthError $ "(.): mismatched lengths of leading/trailing axes of right/left argument"
              | otherwise = (x, y)
          idElem = case f of
              AmbivFn i _ _ -> fromMaybe noIdErr (fnIdAD i)
              DyadFn i _ -> fromMaybe noIdErr (fnIdD i)
          noIdErr = throw . DomainError $ "(.): no identity element for function"
          fd = getDyadFn f
          gd = getDyadFn g
          zipEach :: Array -> [Scalar] -> StateT IdMap IO [Scalar]
          zipEach a ss = arrToList <$> mapVecsAlongAxisM (arrRank a) (\v -> join $ _reduce <$> zipWithM (on gd scalarToArr) v ss) a
          _reduce :: [Array] -> StateT IdMap IO [Scalar]
          _reduce as = (:[]) . arrToScalar <$> foldrM (fd) (last as) (init as)

outerProduct :: Function -> Function
outerProduct f = autoInfoDyadFnM "∘." f _outerProduct
    where _outerProduct x y = partialFlatten . shapedArrFromList shape' <$> mapM (\l -> ((fmap arrToScalar) .: on fd scalarToArr) (l !! 0) (l !! 1)) (sequence [arrToList x, arrToList y])
              where shape' = shape x ++ shape y
                    fd = getDyadFn f

over :: Function -> Function -> Function
over f g = case (f, g) of
    (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (MonFn _ fF, AmbivFn _ gF _) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (DyadFn _ fF, AmbivFn _ gF _) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (AmbivFn _ fMF fDF, MonFn _ gF) -> autoInfoAmbivFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    (AmbivFn _ fMF fDF, AmbivFn _ gF _) -> autoInfoAmbivFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    _ -> throw . SyntaxError $ "(⍥): invalid arity of arguments"
    where overDM d m l r = join $ d <$> m l <*> m r

{- Operators that sometimes take Arrays -}

at :: (Either Array Function) -> (Either Array Function) -> Function
at l r = autoInfoAmbivFnD "@" l r (_at Nothing) (\x -> _at $ Just x)
    where _at mbX y = do selectedIndices <- case r of
                             Right f -> join $ (F.toEvalM . F.where_) . validateBoolArr <$> (getMonFn f) y
                             Left a -> return a
                         iO <- arrToInt <$> getQIo
                         vals <- case l of
                             Right f -> case mbX of
                                 Nothing -> (getMonFn f) . listToArr . map (arrToScalar . evalArrSubscript iO y . (:[]) . Just . scalarToArr_) $ arrToList selectedIndices
                                 Just x -> (getDyadFn f) x . listToArr . map (arrToScalar . evalArrSubscript iO y . (:[]) . Just . scalarToArr_) $ arrToList selectedIndices
                             Left a -> if isJust mbX then throw . SyntaxError $ "(@): ⍺⍺ is array, so derived function is monadic"
                                       else return $ a
                         let vals' = if arrNetSize vals == 1 && arrNetSize selectedIndices > 1
                                     then replicate (arrNetSize selectedIndices) (vals `GrammarTree.at` 0)
                                     else arrToList $ vals
                         let selectedIndices' = map (map (+(-1*iO)). arrToIntVec . scalarToArr) . arrToList $ selectedIndices
                         if not . all (arrIndexInRange y) $ selectedIndices' then throw . LengthError $ "(@): index out of range"
                         else if length vals' == 0 || length selectedIndices' == 0 then return y
                         else if length vals' /= length selectedIndices' then throw . LengthError $ "(@): mismatched lengths of left and right arguments/results"
                         else return $ arrModL y (zipWith (,) selectedIndices' vals')
          validateBoolArr a
              | (\_ -> True) . map scalarToBool . arrToList $ a = a

atop :: (Either Array Function) -> (Either Array Function) -> Function
atop l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (MonFn _ fF, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (MonFn _ fF, AmbivFn _ gMF gDF) -> autoInfoAmbivFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        (AmbivFn _ fF _, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (AmbivFn _ fF _, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (AmbivFn _ fF _, AmbivFn _ gMF gDF) -> autoInfoAmbivFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        _ -> throw . SyntaxError $ "(⍤): invalid arity of argument functions"
    (Right f, Left a) -> autoInfoAmbivFnD "⍤" f a (_rankM f p q r) (_rankD f p q r)
        where (p, q, r) = case arrToIntVec a of
                               (x:[]) -> (x, x, x)
                               (x:y:[]) -> (x, y, x)
                               (x:y:z:[]) -> (x, y, z)
                               _ -> throw . LengthError $ "(⍤): expected rank vector of 1 ≤ length ≤ 3"
    _ -> throw . SyntaxError $ "(⍤): invalid argument types"
    where _rankM f p _ _ y = fmap partialFlatten . join . fmap (F.toEvalM . F.mixLast) . arrMapM (fmap ScalarArr . f' . scalarToArr) . alongRank p $ y
              where f' = getMonFn f
          _rankD f _ q r x y = fmap partialFlatten . join . fmap (F.toEvalM . F.mixLast) $ arrZipWithM (fmap ScalarArr .: on f' scalarToArr) x' y'
              where (x', y') = extend (alongRank q x, alongRank r y)
                    f' = getDyadFn f
          extend (x, y)
              | shape x == [1] = (shapedArrFromList (shape y) $ replicate (arrNetSize y) (x `GrammarTree.at` 0), y)
              | shape y == [1] = (x, shapedArrFromList (shape x) $ replicate (arrNetSize x) (y `GrammarTree.at` 0))
              | shape x == shape y = (x, y)
              | otherwise = throw . LengthError $ "(⍤): mismatched left and right argument shapes"

jot :: (Either Array Function) -> (Either Array Function) -> Function
jot l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (MonFn _ fF, AmbivFn _ gF _) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (DyadFn _ fF, AmbivFn _ gF _) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (AmbivFn _ fMF fDF, MonFn _ gF) -> autoInfoAmbivFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        (AmbivFn _ fMF fDF, AmbivFn _ gF _) -> autoInfoAmbivFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        _ -> throw . SyntaxError $ "(∘): invalid arity of argument functions"
    (Right f, Left a) -> autoInfoMonFnD "∘" f a (curryRight (getDyadFn f) a)
    (Left a, Right f) -> autoInfoMonFnD "∘" a f (curryLeft a (getDyadFn f))
    _ -> throw . SyntaxError $ "(∘): invalid argument types"
    where besideDM d m l r = join $ d <$> return l <*> m r
          curryRight f a x = f x a
          curryLeft a f x = f a x

power :: (Either Array Function) -> (Either Array Function) -> Function
power l r = case (l, r) of
    (Right f, Left a) -> case f of
        (MonFn _ fM) -> autoInfoMonFnD "⍣" l r (_powM f')
                        where f' = if n < 0 then getInverseM f else fM
        (DyadFn _ fD) -> autoInfoDyadFnD "⍣" l r (_powD f')
                         where f' = if n < 0 then getInverseD f else fD
        (AmbivFn _ fM fD) -> autoInfoAmbivFnD "⍣" l r (_powM f'm) (_powD f'd)
                               where (f'm, f'd) = if n < 0 then (getInverseM f, getInverseD f) else (fM, fD)
        where n = arrToInt a
              _powM m x = foldM (flip ($)) x (replicate (abs n) m)
              _powD d x y = foldM (flip ($)) y (replicate (abs n) (d x))
    (Right f, Right g) -> case f of
        MonFn _ fM -> autoInfoMonFnD "⍣" l r (_powM fM)
        DyadFn _ fD -> autoInfoDyadFnD "⍣" l r (_powD fD)
        AmbivFn _ fM fD -> autoInfoAmbivFnD "⍣" l r (_powM fM) (_powD fD)
        where _powM :: FuncM -> FuncM
              _powM m y = do y' <- m y
                             stop <- arrToBool <$> g' y' y
                             if stop then return y' else _powM m y'
              _powD :: FuncD -> FuncD
              _powD d x y = do y' <- d x y
                               stop <- arrToBool <$> g' y' y
                               if stop then return y' else _powD d x y'
              g' = getDyadFn g
    _ -> throw . SyntaxError $ "(⍣): invalid argument types"

selfie :: Either Array Function -> Function
selfie arg = case arg of
    (Left a) -> autoInfoAmbivFnM "⍨" arg (\_ -> return a) (\_ _ -> return a)
    (Right f) -> autoInfoAmbivFnM "⍨" arg (\a -> dyFn a a) (\l r -> dyFn r l)
        where dyFn = getDyadFn f
