{-# LANGUAGE DerivingVia #-}

module Functions where
import Eval
import GrammarTree
import Data.List (elemIndex)
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

{- Constants -}

intMax = maxBound :: Int
floatMax = read "Infinity" :: Double
floatMin = read "-Infinity" :: Double

{- Helpers -}

rankMorph :: (Array, Array) -> (Array, Array)
rankMorph (x, y)
    | shape x == shape y = (x, y)
    | shape x == [1] = (shapedArrFromList (shape y) xs, y)
    | shape y == [1] = (x, shapedArrFromList (shape x) ys)
    | otherwise = throw $ RankError "mismatched ranks (rank morph)"
        where xs = replicate (foldr (*) 1 (shape y)) (at x 0)
              ys = replicate (foldr (*) 1 (shape x)) (at y 0)

alongAxis :: Array -> Int -> [Array]
alongAxis a ax
    | ax - _iO >= (length . shape $ a) = throw $ RankError "invalid axis"
    | (length . shape $ a) == 0 = []
    | foldr (*) 1 (shape a) == 0 = []
    | otherwise = map (subarrayAt) [0..(n - 1)]
    where n = (shape a) !! (ax - _iO)
          shape' = if (length . shape $ a) == 1
                   then [1]
                   else take (ax - _iO) (shape a) ++ drop (ax - _iO + 1) (shape a)
          sz = foldr (*) 1 (shape a)
          subarrayAt i = case map (atl a) $ indicesAt i of
              ((ScalarArr a):[]) -> a
              l -> shapedArrFromList shape' l
          indicesAt i =  map (\is -> take (ax - _iO) is ++ [i] ++ drop (ax - _iO) is) $ map (calcIndex) [0..(sz `div` n - 1)]
          indexMod = tail . reverse $ scanl (*) 1 (reverse shape')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip shape' indexMod
          _iO = 1 -- axis supplied is with respect to _iO = 1, not actual ⎕IO

alongRank :: Array -> Int -> Array
alongRank a r
    | foldr (*) 1 (shape a) == 0 = arrFromList []
    | r <= 0 = a
    | r >= length (shape a) = arrFromList [ScalarArr a]
    | otherwise = shapedArrFromList outerShape . map (ScalarArr . shapedArrFromList innerShape) . groupBy groupSz . arrToList $ a
    where outerShape = take n $ shape a
          innerShape = drop n $ shape a
          n = (length $ shape a) - r
          groupSz = foldr (*) 1 innerShape

arithFnD :: (Double -> Double -> Double) -> Array -> Array -> Array
arithFnD f x' y' = arrZipWith (f') x y
    where (x, y) = rankMorph (x', y')
          f' :: Scalar -> Scalar -> Scalar
          f' (ScalarNum n) (ScalarNum m) = ScalarNum $ f n m
          f' n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ rec (arrFromList [n]) arr
          f' (ScalarArr a) (ScalarArr b) = ScalarArr $ rec a b
          f' (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ rec arr (arrFromList [n])
          f' _ _ = throw $ DomainError "expected number"
          rec = arithFnD f

arithFnM :: (Double -> Double) -> Array -> Array
arithFnM f = arrMap (f')
    where f' (ScalarNum n) = ScalarNum $ f n
          f' (ScalarArr a) = ScalarArr . arithFnM f $ a
          f' _ = throw $ DomainError "expected number"

intToScalarArr :: Int -> Array
intToScalarArr = arrFromList . (:[]) . ScalarNum . fromIntegral

doubleToScalarArr :: Double -> Array
doubleToScalarArr = arrFromList . (:[]) . ScalarNum

doubleToBool :: Double -> Bool
doubleToBool 1.0 = True
doubleToBool 0.0 = False
doubleToBool _ = throw $ DomainError "expected boolean singleton"

boolToDouble :: Bool -> Double
boolToDouble True = 1.0
boolToDouble False = 0.0

scalarToBool :: Scalar -> Bool
scalarToBool (ScalarNum n) = doubleToBool n
scalarToBool _ = throw $ DomainError "expected boolean singleton"

boolToScalar :: Bool -> Scalar
boolToScalar = ScalarNum . boolToDouble

{- Specialized Functions (non-primitive) -}

implicitGroup :: Array -> Array
implicitGroup = id

getString :: StateT IdMap IO Array
getString = arrFromList . map ScalarCh <$> (lift $ getLine)

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

iota :: Array -> IdxOriginM Array
iota x = if any (<0) x'
         then throw $ DomainError "(⍳): expected nonnegative arguments"
         else ask >>= \iO -> return $ shapedArrFromList x' [toScalar . map (ScalarNum . fromIntegral . (+iO)) . calcIndex $ i | i <- [0..(sz - 1)]]
    where x' = arrToIntVec x
          sz = foldr (*) 1 x'
          indexMod = reverse . init $ scanl (*) 1 (reverse x')
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
          xs = alongAxis x 1
          ys = alongRank y (xRank - 1)
          toArray (ScalarArr a) = a
          toArray s = arrFromList [s]

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

lss :: Array -> Array -> Array
lss = arithFnD (\n m -> fromIntegral . fromEnum $ n < m)

match :: Array -> Array -> Array
match x y = intToScalarArr . fromEnum $ (x == y)

minimum :: Array -> Array -> Array
minimum = arithFnD (min)

maximum :: Array -> Array -> Array
maximum = arithFnD (max)

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
