{-# LANGUAGE DerivingVia #-}

module Functions where
import Eval
import GrammarTree
import Data.Fixed (mod')
import Control.Monad
import System.Random
import System.Random.Stateful
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Control.Monad.Trans.State.Strict as StateTStrict
import Exceptions
import Control.Exception (throw)
import Util
import Data.List
import Data.Function
import Data.Maybe
import Lex (tokenize)
import {-# SOURCE #-} Main
import {-# SOURCE #-} Parse

{- SubEvalM (subset of EvalM): typeclass for wrapper monads -}
-- (EvalM here refers to StateT IdMap IO)

class (Monad m) => SubEvalM m where
    toEvalM :: m a -> StateT IdMap IO a

instance SubEvalM Identity where
    toEvalM = return . runIdentity

newtype IdxOriginM a = IdxOriginM { unIdxOriginM :: Reader Int a }
    deriving (Functor, Applicative, Monad, MonadReader Int) via (Reader Int)

instance SubEvalM IdxOriginM where
    toEvalM iom = do
        idm <- get
        let iO = case mapLookup "⎕IO" idm of
                  Just (IdArr a)
                      | ScalarNum n <- a `at` 0 -> Prelude.floor $ n
                  Just _ -> undefined -- unexpected val for ⎕IO
                  _ -> undefined -- no val for ⎕IO
        return . (flip runReader) iO . unIdxOriginM $ iom

newtype RandAndIoM a = RandAndIoM { unRandomAndIoM :: StateTStrict.StateT StdGen (Reader Int) a }
    deriving (Functor, Applicative, Monad, MonadReader Int, MonadState StdGen) via StateTStrict.StateT StdGen (Reader Int)

instance SubEvalM RandAndIoM where
    toEvalM rm = do
        gen <- lift $ newStdGen
        toEvalM . IdxOriginM . runStateGenT_ gen $ \_ -> unRandomAndIoM rm

{- Specialized Functions (non-primitive) -}

implicitGroup :: Array -> Array
implicitGroup = id

getString :: StateT IdMap IO Array
getString = arrFromList . map ScalarCh <$> (lift $ getLine)

{- Axis-Spec Functions -}

partitionedEnclose :: Double -> Array -> Array -> IdxOriginM Array
partitionedEnclose ax x y
    | not . isIntegral $ ax = throw . RankError $ "(⊂): invalid axis"
    | otherwise = do iO <- ask
                     let ax' = (Prelude.floor ax) - iO + 1
                     if ax' <= 0 || ax' > (arrRank y) then throw . RankError $ "(⊂): invalid axis"
                     else if arrRank x /= 1 then throw . RankError $ "(⊂): expected scalar/vector left argument"
                     else return $ _pEnclose ax' (arrToIntVec x) (alongAxis ax' y)
    where _pEnclose :: Int -> [Int] -> [Array] -> Array
          _pEnclose _ [] [] = zilde
          _pEnclose ax' ns es
              | length es == 1 && length ns > 1 = _pEnclose ax' ns (replicate (length ns) (head es))
              | length ns == 1 = _pEnclose ax' (replicate (length es) (head ns)) es
              | length ns > (1 + length es) = throw . LengthError $ "(⊂): left argument too long"
              | any (<0) ns = throw . DomainError $ "(⊂): left argument should be nonnegative"
              | length ns < (1 + length es) = _pEnclose ax' (ns ++ replicate ((1 + length es) - (length ns)) 0) es
              | otherwise = arrFromList . map ScalarArr $ intertwine divGroups arrGroups
                  where ns' = dropWhile (==0) ns -- remove leading zeroes
                        es' = drop (length ns - length ns') es
                        groups = groupBy (\a b -> snd b == 0) $ zip es' ns'
                        arrGroups = map ((:[]) . unAlongAxis ax' . map fst) $ groups
                        divGroups = map ((flip replicate) zilde) $ (map ((-1+) . snd . head) groups) ++ [last ns]
                        intertwine (a:[]) [] = a
                        intertwine (a:as) (b:bs) = a ++ b ++ intertwine as bs
                        intertwine a b = throw . RankError $ (show a) ++ " || " ++ (show b) ++ " // " ++ (show arrGroups) ++ " \\\\ " ++ (show divGroups) ++ " ^^ " ++ (show groups)

partition :: Double -> Array -> Array -> IdxOriginM Array
partition ax x y
    | not . isIntegral $ ax = throw . RankError $ "(⊆): invalid axis"
    | otherwise = do iO <- ask
                     let ax' = (Prelude.floor ax) - iO + 1
                     if ax' <= 0 || ax' > (arrRank y) then throw . RankError $ "(⊆): invalid axis"
                     else if arrRank x /= 1 then throw . RankError $ "(⊆): expected scalar/vector left argument"
                     else return $ mapVecsAlongAxis ax' (_partition (arrToIntVec x)) y
    where _partition :: [Int] -> [Scalar] -> [Scalar]
          _partition [] [] = []
          _partition ns ss
              | length ns == 1 = _partition (replicate (length ss) (head ns)) ss
              | length ns /= length ss = throw . LengthError $ "(⊆): mismatched left and right argument lengths"
              | any (<0) ns = throw . DomainError $ "(⊆): left argument should be nonnegative"
              | otherwise = map joinScalars . filter (/=[]) . map (map fst . filter ((/=0) . snd)) . groupAdj (on (>=) snd) $ zip ss ns
          groupAdj _ [] = []
          groupAdj f (a:as) = case groupAdj f as of
                                  [] -> [[a]]
                                  rest@(g:gs)
                                      | f a (head g) -> (a : g) : gs
                                      | otherwise -> [a] : rest
          joinScalars ss = ScalarArr . arrFromList $ ss

reverse :: Double -> Array -> IdxOriginM Array
reverse ax x
    | not . isIntegral $ ax = throw . RankError $ "(⌽): invalid axis"
    | otherwise = ask >>= \iO -> let ax' = (Prelude.floor ax) - iO + 1
                                 in if ax' <= 0 || ax' > (arrRank x)
                                    then throw . RankError $ "(⌽): invalid axis"
                                    else return $ mapVecsAlongAxis ax' Prelude.reverse x

rotate :: Double -> Array -> Array -> IdxOriginM Array
rotate ax x y
    | not . isIntegral $ ax = throw . DomainError $ "(⌽): invalid axis"
    | otherwise = ask >>= \iO -> return $ _rotate iO
        where _rotate iO
                  | ax' < iO || ax' > (iO + (arrRank y) - 1) = throw . RankError $ "(⌽): invalid axis"
                  | shape x' /= shape' = throw . LengthError $ "(⌽): invalid shape of left argument"
                  | otherwise = zipVecsAlongAxis (length $ shape x'') ax' ax' __rotate x'' y
                  where ax' = Prelude.floor ax - iO + 1
                        x' = case shape x of -- extend x if scalar
                            [1] -> shapedArrFromList shape' (replicate (foldr (*) 1 shape') (x `at` 0))
                            _ -> x
                        shape' = if arrRank y == 1
                                 then [1]
                                 else take (ax' - 1) (shape y) ++ drop ax' (shape y)
                        x'' = x' { shape = (shape x') ++ [1] } -- append a dummy axis
              __rotate a b = case map scalarToInt a of
                  [n] -> if n > 0
                         then drop n' b ++ take n' b
                         else Prelude.reverse $ drop n' b' ++ take n' b'
                         where n' = (abs n) `mod` length b
                               b' = Prelude.reverse b
                  _ -> throw . DomainError $ "(⌽): expected scalar array as left argument"

{- First/Last -Axis Functions -}

partitionLast :: Array -> Array -> IdxOriginM Array
partitionLast x y = ask >>= \iO -> Functions.partition (fromIntegral $ iO + (arrRank y) - 1) x y

partitionedEncloseLast :: Array -> Array -> IdxOriginM Array
partitionedEncloseLast x y = ask >>= \iO -> partitionedEnclose (fromIntegral $ iO + (arrRank y) - 1) x y

reverseFirst :: Array -> IdxOriginM Array
reverseFirst x = ask >>= \iO -> Functions.reverse (fromIntegral iO) x

reverseLast :: Array -> IdxOriginM Array
reverseLast x = ask >>= \iO -> Functions.reverse (fromIntegral $ iO + (arrRank x) - 1) x

rotateFirst :: Array -> Array -> IdxOriginM Array
rotateFirst x y = ask >>= \iO -> rotate (fromIntegral iO) x y

rotateLast :: Array -> Array -> IdxOriginM Array
rotateLast x y = ask >>= \iO -> rotate (fromIntegral $ iO + (arrRank y) - 1) x y

{- EvalM Functions -}

execute :: Array -> StateT IdMap IO Array
execute x = _execStatement . tokenize . arrToString $ x
    where _handleRes :: ExprResult -> StateT IdMap IO ()
          _handleRes res = do idm <- get
                              mb <- lift . catchExecErr $ evalAndShowRes idm res
                              case mb of
                                  Nothing -> throw . SyntaxError $ "(⍎): execution failed"
                                  Just idm' -> put idm'
          _execStatement ts = do idm <- get
                                 case evalMatchFn idm ts parseExpr of
                                     Nothing -> throw . SyntaxError $ "(⍎): Parse Error"
                                     Just (res, ts') -> case ts' of
                                         [] -> case res of
                                                   ResNull -> return zilde
                                                   ResFtn _ _ -> _handleRes res >> return zilde
                                                   ResOtn _ _ -> _handleRes res >> return zilde
                                                   ResAtn atn _ -> evalArrTree atn
                                         _ -> _handleRes res >> _execStatement ts'

{- Rng Functions -}

roll :: Array -> RandAndIoM Array
roll a = shapedArrFromList (shape a) <$> mapM (_mapF) (arrToList a)
    where _mapF :: Scalar -> RandAndIoM Scalar
          _mapF s = case s of
              ScalarArr a -> ScalarArr <$> roll a
              ScalarNum n -> ScalarNum <$> _roll n
              _ -> throw $ DomainError "expected number"
          _roll :: Double -> RandAndIoM Double
          _roll n
              | not . isIntegral $ n = throw $ DomainError "expected integral number"
              | n < 0 = throw $ DomainError "(?): arguments to must be nonnegative"
              | n == 0 = uniformRM (0.0, 1.0) StateGenM
              | otherwise = ask >>= \iO -> fromIntegral <$> uniformRM (iO, Prelude.floor n) StateGenM

deal :: Array -> Array -> RandAndIoM Array
deal x y
    | x' > y' = throw $ DomainError "(?): right argument to must be greater than or equal to the right"
    | otherwise = arrFromList . map (ScalarNum . fromIntegral) <$> _deal [(1, y')] y' x'
    where x' = arrToInt x
          y' = arrToInt y
          _deal ranges netRange 0 = return []
          _deal ranges netRange n = do
              (ranges', x) <- dealOne ranges netRange
              (x:) <$> _deal ranges' (netRange - 1) (n - 1)
          dealOne ranges netRange = do
              x <- uniformRM (1, netRange) StateGenM
              return $ findAndRemove ranges x
          findAndRemove [] _ = undefined -- this shouldn't happen
          findAndRemove ((r@(start, len)):rs) x
              | x == len = if len == 1
                           then (rs, start + x - 1)
                           else ((start, len - 1) : rs, start + x - 1)
              | x < len = if x == 1
                          then ((start + 1, len - 1) : rs, start + x - 1)
                          else ((start, x - 1) : (start + x, len - x) : rs, start + x - 1)
              | otherwise = let (rs', x') = findAndRemove rs (x - len)
                            in (r:rs', x')

{- ⎕IO Functions -}

gradeUpD :: Array -> Array -> IdxOriginM Array
gradeUpD x y
    | not (all isChar xList && all isChar yList) = throw . DomainError $ "(⍋): args must be simple character arrays"
    | not (arrRank x == 1) = throw . RankError $ "(⍋): expected vector as left argument"
    | otherwise = ask >>= \iO -> return . arrFromList . map (ScalarNum . fromIntegral . fst) . sortBy (on _cmp snd) . zipWith (,) [iO..] . alongAxis iO $ y
    where isChar s
              | ScalarCh _ <- s = True
              | otherwise = False
          xList = arrToList x
          yList = arrToList y
          _cmp = on compare (map (_ind xList) . arrToList)
          _ind l e = fromMaybe (length l) . lookup e $ zip l [0..]

gradeUpM :: Array -> IdxOriginM Array
gradeUpM x = ask >>= \iO -> return . arrFromList . map (ScalarNum . fromIntegral . fst) . sortBy (on compare snd) . zipWith (,) [iO..] . alongAxis_ iO $ x

gradeDownD :: Array -> Array -> IdxOriginM Array
gradeDownD x y
    | not (all isChar xList && all isChar yList) = throw . DomainError $ "(⍒): args must be simple character arrays"
    | not (arrRank x == 1) = throw . RankError $ "(⍒): expected vector as left argument"
    | otherwise = ask >>= \iO -> return . arrFromList . map (ScalarNum . fromIntegral . fst) . sortBy (on (flip _cmp) snd) . zipWith (,) [iO..] . alongAxis iO $ y
    where isChar s
              | ScalarCh _ <- s = True
              | otherwise = False
          xList = arrToList x
          yList = arrToList y
          _cmp = on compare (map (_ind xList) . arrToList)
          _ind l e = fromMaybe (length l) . lookup e $ zip l [0..]

gradeDownM :: Array -> IdxOriginM Array
gradeDownM x = ask >>= \iO -> return . arrFromList . map (ScalarNum . fromIntegral . fst) . sortBy (on (flip compare) snd) . zipWith (,) [iO..] . alongAxis_ iO $ x

iota :: Array -> IdxOriginM Array
iota x = if any (<0) x'
         then throw $ DomainError "(⍳): expected nonnegative arguments"
         else ask >>= \iO -> return $ shapedArrFromList x' [toScalar . map (ScalarNum . fromIntegral . (+iO)) . calcIndex $ i | i <- [0..(sz - 1)]]
    where x' = arrToIntVec x
          sz = foldr (*) 1 x'
          indexMod = Prelude.reverse . init $ scanl (*) 1 (Prelude.reverse x')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip x' indexMod
          toScalar (s:[]) = s
          toScalar (ss) = ScalarArr . arrFromList $ ss

indexOf :: Array -> Array -> IdxOriginM Array
indexOf x y
    | xRank > yRank = throw $ RankError "(⍳): mismatched ranks"
    | xRank == 1 && (head . shape $ x) <= 1 = throw $ RankError "(⍳): left argument to cannot be a scalar"
    | (tail . shape $ x) /= (drop (1 + yRank - xRank) . shape $ y) = throw $ LengthError "(⍳): mismatched argument lengths"
    | otherwise = do
          iO <- ask
          let findIndexInXs e = case elemIndex (toArray e) xs of
                  Nothing -> ScalarNum . fromIntegral $ iO + (head . shape $ x)
                  Just i -> ScalarNum . fromIntegral $ iO + i
          return $ arrMap (findIndexInXs) ys
    where xRank = length . shape $ x
          yRank = length . shape $ y
          xs = alongAxis_ 1 x
          ys = alongRank (xRank - 1) y
          toArray (ScalarArr a) = a
          toArray s = arrFromList [s]

pick :: Array -> Array -> IdxOriginM Array
pick x y
    | arrRank x /= 1 = throw . RankError $ "(⊃): expected vector as left argument"
    | otherwise = ask >>= \iO -> return $ _pick ((map . map) (+(-1*iO)) indices) y
    where indices = map (arrToIntVec . scalarToArr) . arrToList $ x
          _pick [] a = a
          _pick (i:is) a
              | length i /= arrRank a = throw . RankError $ "(⊃): invalid length of index"
              | not $ arrIndexInRange a i = throw . LengthError $ "(⊃): index out of range"
              | otherwise = _pick is a'
                  where a' = case arrIndex a i of
                                 ScalarArr arr -> arr
                                 s -> arrFromList [s]

reorderAxes :: Array -> Array -> IdxOriginM Array
reorderAxes x y
    | arrRank x /= 1 = throw . RankError $ "(⍉): invalid rank of left argument"
    | (head . shape) x /= arrRank y = throw . RankError $ "(⍉): invalid length of left argument"
    | otherwise = ask >>= \iO -> if (not . all (`elem`x')) [iO..(foldr (max) iO x')]
                                 || (not . all (`elem`[iO..(foldr (max) iO x')])) x'
                                 then throw . RankError $ "(⍉): invalid axis"
                                 else return $ arrReorderAxes x' y
    where x' = arrToIntVec x

{- Pure Functions -}

absoluteValue :: Array -> Array
absoluteValue = arithFnM (abs)

add :: Array -> Array -> Array
add = arithFnD (+)

binomial :: Array -> Array -> Array
binomial = arithFnD _binomial
    where _binomial x y
              | x /= (fromIntegral . Prelude.floor $ x) = throw $ DomainError "(!): arguments should be integral"
              | y /= (fromIntegral . Prelude.floor $ y) = throw $ DomainError "(!): arguments should be integral"
              | x < 0 = throw $ DomainError "(!): arguments should be positive"
              | y < 0 = throw $ DomainError "(!): arguments should be positive"
              | otherwise = (flip _binomialRec) (Prelude.floor x) (Prelude.floor y)
          _binomialRec x y
              | y > x = 0
              | y == 0 = 1
              | x == y = 1
              | otherwise = _binomialRec (x - 1) (y - 1) + _binomialRec (x - 1) y

ceiling :: Array -> Array
ceiling = arithFnM (fromIntegral . Prelude.ceiling)

circularFormulae :: Array -> Array -> Array
circularFormulae = arithFnD _cf
    where _cf x y
              | x == 1 = sin y
              | x == 2 = cos y
              | x == 3 = tan y
              | x == -1 && y >= -1 && y <= 1 = asin y
              | x == -1 = throw $ DomainError "(¯1○): asin out of range"
              | x == -2 && y >= -1 && y <= 1 = acos y
              | x == -2 = throw $ DomainError "(¯2○): acos out of range"
              | x == -3 = atan y
              | otherwise = throw $ DomainError "(○): lhs not in {¯3, ¯2, ¯1, 1, 2, 3}"

conjugate :: Array -> Array
conjugate = id

depth :: Array -> Array
depth = intToScalarArr . _depth . ScalarArr
    where _depth :: Scalar -> Int
          _depth (ScalarArr a) = mult * (1 + (foldr (max) 0 . map abs) childDepths)
              where childDepths = map (_depth) . arrToList $ a
                    mult = if allMatch childDepths && all (>=0) childDepths then 1 else -1
                    allMatch (x:x':xs)
                        | x /= x' = False
                        | otherwise = allMatch (x':xs)
                    allMatch _ = True
          _depth _ = 0

direction :: Array -> Array
direction = arithFnM _direction
    where _direction n
              | n < 0 = -1
              | n == 0 = 0
              | n > 0 = 1

divide :: Array -> Array -> Array
divide = arithFnD _divide
    where _divide _ 0 = throw $ DomainError "division by zero"
          _divide n m = n / m

encode :: Array -> Array -> Array
encode x y = arrReorderAxes reorderedAxes . shapedArrFromList shape' . concat . map arrToList $ encodings
    where y' = arrToIntVec y
          shape' = (shape_ y) ++ (shape x)
          encodings = map (\i -> mapVecsAlongAxis 1 (_encode i) x) y'
          _encode :: Int -> [Scalar] -> [Scalar]
          _encode i scs = map (ScalarNum . fromIntegral) $ zipWith (\r d -> i `_div` d `_mod` r) radixes divisors
              where radixes = map scalarToInt scs
                    divisors = tail $ scanr (*) 1 radixes
                    _div x y
                        | y == 0 = 0
                        | otherwise = div x y
                    _mod x y
                        | y == 0 = x
                        | otherwise = mod x y
          reorderedAxes = ([i + (length $ shape x) | i <- [1..(length $ shape_ y)]] ++ [1..(length $ shape x)])

enclose :: Array -> Identity Array
enclose x
    | (shape x) == [1] && (not . isScalarArr) (x `at` 0) = return x
    | otherwise = return . arrFromList . (:[]) . ScalarArr $ x

enlist :: Array -> Array
enlist = arrFromList . concat . map _flatten . arrToList
    where _flatten s = case s of
              (ScalarArr a) -> concat . map _flatten . arrToList $ a
              _ -> [s]

equ :: Array -> Array -> Array
equ = arithFnD (\n m -> fromIntegral . fromEnum $ n == m)

exponential :: Array -> Array
exponential = arithFnM (exp)

factorial :: Array -> Array
factorial = arithFnM (_factorial)
    where _factorial :: Double -> Double
          _factorial n
              | n /= (fromIntegral . Prelude.floor $ n) = throw $ DomainError "(!): arguments should be integral"
              | n < 0 = throw $ DomainError "(!): arguments should be nonnegative"
              | otherwise = fromIntegral . foldr (*) 1 $ [1..(Prelude.floor n)]

first :: Array -> Identity Array
first x = case arrToList x of
    [] -> return $ doubleToScalarArr 0
    x':_ -> case x' of
                ScalarArr a -> return $ a
                _ -> return $ arrFromList [x']

format :: Array -> Array
format x = case map (map ScalarCh) . lines . show $ x of
    [] -> zilde
    (v:[]) -> arrFromList v
    x' -> shapedArrFromList [length x'', length (head x'')] . concat $ x''
        where _pad sss = map (\ss -> ss ++ replicate (padSz - length ss) (ScalarCh ' ')) sss
                  where padSz = foldr (max) 0 . map length $ sss
              x'' = _pad x'

floor :: Array -> Array
floor = arithFnM (fromIntegral . Prelude.floor)

gcd :: Array -> Array -> Array
gcd = arithFnD (_gcd)
    where _gcd x y
              | x /= (fromIntegral . Prelude.floor $ x) = throw $ DomainError "(∨): arguments should be integral"
              | y /= (fromIntegral . Prelude.floor $ y) = throw $ DomainError "(∨): arguments should be integral"
              | otherwise = fromIntegral $ Prelude.gcd (Prelude.floor x) (Prelude.floor y)

geq :: Array -> Array -> Array
geq = arithFnD (\n m -> fromIntegral . fromEnum $ n >= m)

gtr :: Array -> Array -> Array
gtr = arithFnD (\n m -> fromIntegral . fromEnum $ n > m)

identity :: Array -> Array
identity = id

intersection :: Array -> Array -> Array
intersection x y
    | arrRank x /= 1 = throw . DomainError $ "(∩): left argument should be a vector"
    | arrRank y /= 1 = throw . DomainError $ "(∩): right argument should be a vector"
    | otherwise = arrFromList . filter (`elem`yList) $ xList
    where xList = arrToList x
          yList = arrToList y

lcm :: Array -> Array -> Array
lcm = arithFnD (_lcm)
    where _lcm x y
              | x /= (fromIntegral . Prelude.floor $ x) = throw $ DomainError "(∧): arguments should be integral"
              | y /= (fromIntegral . Prelude.floor $ y) = throw $ DomainError "(∧): arguments should be integral"
              | x < 0 && y >= 0 = -1 * _lcm (-1 * x) y
              | y < 0 && x >= 0 = -1 * _lcm x (-1 * y)
              | otherwise = fromIntegral $ Prelude.lcm (Prelude.floor x) (Prelude.floor y)

left :: Array -> Array -> Array
left = const

leq :: Array -> Array -> Array
leq = arithFnD (\n m -> fromIntegral . fromEnum $ n <= m)

logBase :: Array -> Array -> Array
logBase = arithFnD (_logBase)
    where _logBase b n | b <= 0 || b == 1 || n <= 0 = throw $ DomainError "(⍟)"
          _logBase b n = Prelude.logBase b n

logicalNegate :: Array -> Array
logicalNegate = arrMap _not
    where _not s = case s of
              (ScalarArr a) -> ScalarArr $ arrMap _not a
              _ -> boolToScalar . not . scalarToBool $ s

lss :: Array -> Array -> Array
lss = arithFnD (\n m -> fromIntegral . fromEnum $ n < m)

match :: Array -> Array -> Array
match x y = intToScalarArr . fromEnum $ (x == y)

minimum :: Array -> Array -> Array
minimum = arithFnD (min)

maximum :: Array -> Array -> Array
maximum = arithFnD (max)

membership :: Array -> Array -> Array
membership x y = arrMap (boolToScalar . _isElement) x
    where _isElement s = s `elem` yList
          yList = arrToList y

multiply :: Array -> Array -> Array
multiply = arithFnD (*)

nand :: Array -> Array -> Array
nand = arithFnD (\x y -> boolToDouble $ not (doubleToBool x && doubleToBool y))

naturalLog :: Array -> Array
naturalLog = arithFnM (_log)
    where _log n | n <= 0 = throw $ DomainError "(⍟)"
          _log n = log n

negate :: Array -> Array
negate = arithFnM (Prelude.negate)

neq :: Array -> Array -> Array
neq = arithFnD (\n m -> fromIntegral . fromEnum $ n /= m)

nest :: Array -> Identity Array
nest x
    | any isScalarArr $ arrToList x = return x
    | shape x == [1] = return x
    | otherwise = return $ arrFromList [ScalarArr x]

nor :: Array -> Array -> Array
nor = arithFnD (\x y -> boolToDouble $ not (doubleToBool x || doubleToBool y))

notMatch :: Array -> Array -> Array
notMatch x y = intToScalarArr . fromEnum $ (x /= y)

piTimes :: Array -> Array
piTimes = arithFnM (pi*)

power :: Array -> Array -> Array
power = arithFnD (**)

reciprocal :: Array -> Array
reciprocal = arithFnM (_reciprocal)
    where _reciprocal 0 = throw $ DomainError "division by zero"
          _reciprocal n = 1 / n

reshape :: Array -> Array -> Array
reshape x y = shapedArrFromList newShape . take newSize . concat . replicate intMax $ baseList
    where newShape = arrToIntVec x
          newSize = foldr (*) 1 newShape
          baseList = case arrToList y of
                     [] -> [ScalarNum 0]
                     ss -> ss

residue :: Array -> Array -> Array
residue = arithFnD (_residue)
    where _residue 0 m = 0
          _residue n m =  mod' m n

right :: Array -> Array -> Array
right = flip const

shapeOf :: Array -> Array
shapeOf = arrFromList . map (ScalarNum . fromIntegral) . shape

subtract :: Array -> Array -> Array
subtract = arithFnD (-)

tally :: Array -> Array
tally a
    | (length . shape $ a) == 0 = intToScalarArr 0
    | otherwise = intToScalarArr . head . shape $ a

transpose :: Array -> Identity Array
transpose x = Identity $ arrReorderAxes (Prelude.reverse [1..(length $ shape x)]) x

union :: Array -> Array -> Array
union x y
    | arrRank x /= 1 = throw . DomainError $ "(∪): left argument should be a vector"
    | arrRank y /= 1 = throw . DomainError $ "(∪): right argument should be a vector"
    | otherwise = arrFromList $ xList ++ filter (not . (`elem`xList)) yList
    where xList = arrToList x
          yList = arrToList y

unique :: Array -> Array
unique = unAlongAxis 1 . nub . alongAxis 1

uniqueMask :: Array -> Array
uniqueMask = arrFromList . map (boolToScalar) . _uniqMask . alongAxis 1
    where _uniqMask = Prelude.reverse . _uniqMaskRec . Prelude.reverse
          _uniqMaskRec (x:xs) = not (x `elem` xs) : _uniqMaskRec xs
          _uniqMaskRec [] = []

without :: Array -> Array -> Array
without x y
    | arrRank x /= 1 = throw . DomainError $ "(~): left argument should be a vector"
    | arrRank y /= 1 = throw . DomainError $ "(~): right argument should be a vector"
    | otherwise = arrFromList . filter (not . (`elem`yList)) $ xList
    where xList = arrToList x
          yList = arrToList y
