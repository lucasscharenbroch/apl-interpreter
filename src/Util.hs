module Util where
import GrammarTree
import Control.Exception
import Exceptions

{- Composition Operators -}
-- there are libraies for this, but these are too easy to write to bother downloading them

infixr 8 .:
infixr 8 .:.

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.) . (.)

{- Constants -}

intMax = maxBound :: Int
floatMax = read "Infinity" :: Double
floatMin = read "-Infinity" :: Double

{- General -}

isIntegral :: Double -> Bool
isIntegral n = n == (fromIntegral . Prelude.floor $ n)


{- Array Helpers (low-level) -}

arrCat :: Array -> Array -> Array
arrCat x y = arrFromList $ (arrToList x) ++ (arrToList y)

arrCons :: Scalar -> Array -> Array
arrCons x y = arrFromList $ x : (arrToList y)

arrZipWith :: (Scalar -> Scalar -> Scalar) -> Array -> Array -> Array
arrZipWith f x y
    | xsz /= ysz = undefined
    | otherwise = shapedArrFromList (shape x) [(x `at` i) `f` (y `at` i) | i <- [0..xsz]]
    where xsz = foldr (*) 1 $ shape x
          ysz = foldr (*) 1 $ shape y

arrZipWithM :: Monad m => (Scalar -> Scalar -> m Scalar) -> Array -> Array -> m Array
arrZipWithM f x y
    | xsz /= ysz = undefined
    | otherwise = shapedArrFromList (shape x) <$> sequence [(x `at` i) `f` (y `at` i) | i <- [0..xsz]]
    where xsz = foldr (*) 1 $ shape x
          ysz = foldr (*) 1 $ shape y

arrMap :: (Scalar -> Scalar) -> Array -> Array
arrMap f a = shapedArrFromList (shape a) . map (f) . arrToList $ a

arrMapM :: Monad m => (Scalar -> m Scalar) -> Array -> m Array
arrMapM f a = shapedArrFromList (shape a) <$> mapM f (arrToList a)

arrRank :: Array -> Int
arrRank a = length . shape $ a

arrNetSize :: Array -> Int
arrNetSize = foldr (*) 1 . shape

arrIndex :: Array -> [Int] -> Scalar
arrIndex a is = a `at` sum (zipWith (*) is indexMod)
    where indexMod = reverse . init . scanl (*) 1 . reverse . shape $ a
    -- where indexMod = tail . scanr (*) . shape $ a

arrToDouble :: Array -> Double
arrToDouble a
    | shape a /= [1] = throw $ DomainError "expected numeric singleton"
    | otherwise = case a `at` 0 of
                      ScalarNum n -> n
                      _ -> throw $ DomainError "expected numeric scalar"

arrToInt :: Array -> Int
arrToInt a
    | not . isIntegral $ n = throw $ DomainError "expected intergral singleton"
    | otherwise = Prelude.floor $ n
    where n = arrToDouble a

arrToIntVec :: Array -> [Int]
arrToIntVec = map (toInt) . arrToList
    where toInt (ScalarNum n) | (fromIntegral . Prelude.floor $ n) - n == 0 = Prelude.floor n
          toInt _ = throw $ DomainError "expected int singleton"

-- remove redundant `1`s from shape.
partialFlatten :: Array -> Array
partialFlatten a = a { shape = _flatten (shape a) }
    where _flatten [] = []
          _flatten (1:x:xs) = _flatten (x : xs)
          _flatten (x:1:xs) = _flatten (x : xs)
          _flatten (x:xs) = x : _flatten xs


{- Array-Related Helpers (high-level) -}

rankMorph :: (Array, Array) -> (Array, Array)
rankMorph (x, y) -- a.k.a. "scalar extension"
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
          subarrayAt i = case map (arrIndex a) $ indicesAt i of
              ((ScalarArr a):[]) -> a
              l -> shapedArrFromList shape' l
          indicesAt i =  map (\is -> take (ax - _iO) is ++ [i] ++ drop (ax - _iO) is) $ map (calcIndex) [0..(sz `div` n - 1)]
          indexMod = tail . reverse $ scanl (*) 1 (reverse shape')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip shape' indexMod
          _iO = 1 -- axis supplied is with respect to _iO = 1, not actual ⎕IO

alongRank :: Array -> Int -> Array
alongRank a r
    | foldr (*) 1 (shape a) == 0 = arrFromList []
    | n <= 0 = arrFromList [ScalarArr a]
    | n >= (length $ shape a) = a
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

arrToBool :: Array -> Bool
arrToBool a = case arrToInt a of
    1 -> True
    0 -> False
    _ -> throw . DomainError $ "expected boolean singleton"
