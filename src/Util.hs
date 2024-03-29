module Util where
import GrammarTree
import Control.Exception
import Exceptions
import Data.List
import Data.Function

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
zilde = listToArr []

{- General -}

isIntegral :: Double -> Bool
isIntegral n = n == (fromIntegral . floor $ n)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n x = [take n x] ++ groupsOf n (drop n x)

windowsOf :: Int -> [a] -> [[a]]
windowsOf n a
    | n > length a = []
    | otherwise = (take n a) : windowsOf n (tail a)

{- Array Helpers (low-level) -}

arrCat :: Array -> Array -> Array
arrCat x y = listToArr $ (arrToList x) ++ (arrToList y)

arrCons :: Scalar -> Array -> Array
arrCons x y = listToArr $ x : (arrToList y)

arrZipWith :: (Scalar -> Scalar -> Scalar) -> Array -> Array -> Array
arrZipWith f x y
    | xsz /= ysz = undefined
    | otherwise = shapedArrFromList (shape x) [(x `at` i) `f` (y `at` i) | i <- [0..(xsz - 1)]]
    where xsz = foldr (*) 1 $ shape x
          ysz = foldr (*) 1 $ shape y

arrZipWithM :: Monad m => (Scalar -> Scalar -> m Scalar) -> Array -> Array -> m Array
arrZipWithM f x y
    | xsz /= ysz = undefined
    | otherwise = shapedArrFromList (shape x) <$> sequence [(x `at` i) `f` (y `at` i) | i <- [0..(xsz - 1)]]
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
    where indexMod = tail . scanr (*) 1 $ shape a

arrModL :: Array -> [([Int], Scalar)] -> Array
arrModL a l = arrMod a (map (\(is, e) -> (sum $ zipWith (*) is indexMod, e)) l)
    where indexMod = tail . scanr (*) 1 $ shape a

arrIndexInRange :: Array -> [Int] -> Bool
arrIndexInRange a is = all (>=0) is && all id (zipWith (<) is (shape a)) && arrRank a == length is

arrToDouble :: Array -> Double
arrToDouble a
    | shape a /= [1] = throw $ DomainError "expected numeric singleton"
    | otherwise = case a `at` 0 of
                      ScalarNum n -> n
                      _ -> throw $ DomainError "expected numeric scalar"

arrToInt :: Array -> Int
arrToInt a
    | not . isIntegral $ n = throw $ DomainError "expected intergral singleton"
    | otherwise = floor $ n
    where n = arrToDouble a

arrToIntVec :: Array -> [Int]
arrToIntVec = map (toInt) . arrToList
    where toInt (ScalarNum n) | (fromIntegral . floor $ n) - n == 0 = floor n
          toInt _ = throw $ DomainError "expected int singleton"

-- remove redundant `1`s from shape.
partialFlatten :: Array -> Array
partialFlatten a = a { shape = _flatten (shape a) }
    where _flatten [] = []
          _flatten (1:x:xs) = _flatten (x : xs)
          _flatten (x:1:xs) = _flatten (x : xs)
          _flatten (x:xs) = x : _flatten xs

shape_ :: Array -> [Int]
shape_ a
    | shape a == [1] = []
    | otherwise = shape a

{- Array-Related Helpers (high-level) -}

rankMorph :: (Array, Array) -> (Array, Array)
rankMorph (x, y) -- a.k.a. "scalar extension"
    | shape x == shape y = (x, y)
    | shape x == [1] = (shapedArrFromList (shape y) xs, y)
    | shape y == [1] = (x, shapedArrFromList (shape x) ys)
    | otherwise = throw $ RankError "mismatched ranks (rank morph)"
        where xs = replicate (foldr (*) 1 (shape y)) (at x 0)
              ys = replicate (foldr (*) 1 (shape x)) (at y 0)

alongAxis :: Int -> Array -> [Array]
alongAxis ax a
    | ax - _iO >= (length . shape $ a) = throw $ RankError "invalid axis"
    | (length . shape $ a) == 0 = []
    | arrNetSize a == 0 = []
    | otherwise = map (subarrayAt) [0..(n - 1)]
        where n = (shape a) !! (ax - _iO)
              shape' = if (length . shape $ a) == 1
                       then [1]
                       else take (ax - _iO) (shape a) ++ drop (ax - _iO + 1) (shape a)
              sz = arrNetSize a
              subarrayAt i = shapedArrFromList shape' . map (arrIndex a) $ indicesAt i
              indicesAt i =  map (\is -> take (ax - _iO) is ++ [i] ++ drop (ax - _iO) is) $ map (calcIndex) [0..(sz `div` n - 1)]
              indexMod = tail . scanr (*) 1 $ shape'
              calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip shape' indexMod
              _iO = 1 -- axis supplied is with respect to _iO = 1, not actual ⎕IO

alongAxis_ :: Int -> Array -> [Array]
alongAxis_ ax a = map unwrap $ alongAxis ax a
    where unwrap x
              | shape x /= [1] = x
              | (ScalarArr a:[]) <- arrToList x = a
              | otherwise = x

unAlongAxis :: Int -> [Array] -> Array
unAlongAxis ax subArrs
    | length subArrs == 0 = zilde
    | not . all (==(shape . head $ subArrs)) $ map (shape) subArrs = undefined
    | otherwise = shapedArrFromList shape' $ map _elemAt [0..(n - 1)]
        where _shape = shape . head $ subArrs
              shape' = if _shape == [1]
                      then [length subArrs]
                      else take ax' (_shape) ++ [length subArrs] ++ drop ax' (_shape)
              _iO = 1 -- axis supplied is with respect to _iO = 1, not actual ⎕IO
              indexMod = tail . scanr (*) 1 $ shape'
              calcIndex i = zipWith (\e m -> (i `div` m) `mod` e) shape' indexMod
              n = foldr (*) 1 shape'
              _elemAt i = (subArrs !! (calcIndex i !! ax')) `arrIndex` idxList'
                  where idxList = calcIndex i
                        subArrI = idxList !! ax'
                        idxList' = take ax' idxList ++ drop (ax' + 1) idxList
              ax' = ax - _iO

vecsAlongAxis :: Int -> Array -> [Array]
vecsAlongAxis ax a = map listToArr $ vecs
    where subArrs = alongAxis ax a
          subArrShape = if length subArrs == 0 then [0] else shape (head subArrs)
          subArrNetSz = foldr (*) 1 subArrShape
          vecs = foldr (zipWith (:)) (replicate subArrNetSz []) $ map arrToList subArrs

mapVecsAlongAxis :: Int -> ([Scalar] -> [Scalar]) -> Array -> Array
mapVecsAlongAxis ax f a = if length subArrs' == 1 then subArrs' !! 0 else unAlongAxis ax subArrs'
    where subArrs = alongAxis ax a
          subArrShape = if length subArrs == 0 then [0] else shape (head subArrs)
          subArrNetSz = foldr (*) 1 subArrShape
          vecs = foldr (zipWith (:)) (replicate subArrNetSz []) $ map arrToList subArrs
          vecs' = map f vecs
          subArrs' = map (shapedArrFromList subArrShape) $ foldr (zipWith (:)) (replicate vecSz []) vecs' -- undo the zipping into vecs
          vecSz = case length vecs' of
              0 -> 0
              _ -> case map (length) vecs' of
                  x | all (==(head x)) x -> head x
                  _ -> undefined -- f should yeild vectors of uniform size

mapVecsAlongAxisM :: Monad m => Int -> ([Scalar] -> m [Scalar]) -> Array -> m Array
mapVecsAlongAxisM ax f a = do res <- subArrs'
                              if length res == 1
                              then return (res !! 0)
                              else return $ unAlongAxis ax res
    where subArrs = alongAxis ax a
          subArrShape = if length subArrs == 0 then [0] else shape (head subArrs)
          subArrNetSz = foldr (*) 1 subArrShape
          vecs = foldr (zipWith (:)) (replicate subArrNetSz []) $ map arrToList subArrs
          vecs' = mapM f vecs
          subArrs' = do vecs'' <- vecs'
                        let vecSz = case length vecs'' of
                                0 -> 0
                                _ -> case map (length) vecs'' of
                                    x | all (==(head x)) x -> head x
                                    _ -> undefined -- f should yeild vectors of uniform size
                        return . map (shapedArrFromList subArrShape) . foldr (zipWith (:)) (replicate vecSz []) $ vecs''

zipVecsAlongAxis :: Int -> Int -> Int -> ([Scalar] -> [Scalar] -> [Scalar]) -> Array -> Array -> Array
zipVecsAlongAxis axA axB axC f a b = if length subArrs' == 1 then subArrs' !! 0 else unAlongAxis axC subArrs'
    where subArrsA = alongAxis axA a
          subArrsB = alongAxis axB b
          subArrShapeA = if length subArrsA == 0 then [0] else shape (head subArrsA)
          subArrShapeB = if length subArrsB == 0 then [0] else shape (head subArrsB)
          subArrNetSzA = foldr (*) 1 subArrShapeA
          subArrNetSzB = foldr (*) 1 subArrShapeB
          vecsA = foldr (zipWith (:)) (replicate subArrNetSzA []) $ map arrToList subArrsA
          vecsB = foldr (zipWith (:)) (replicate subArrNetSzB []) $ map arrToList subArrsB
          vecs' = zipWith f vecsA vecsB
          subArrs' = map (shapedArrFromList subArrShapeA) $ foldr (zipWith (:)) (replicate vecSz []) vecs' -- undo the zipping into vecs
          vecSz = case length vecs' of
              0 -> 0
              _ -> case map (length) vecs' of
                  x | all (==(head x)) x -> head x
                  _ -> undefined -- f should yeild vectors of uniform size

alongRank :: Int -> Array -> Array
alongRank r a
    | foldr (*) 1 (shape a) == 0 = zilde
    | n <= 0 = listToArr [ScalarArr a]
    | n >= (length $ shape a) = a
    | otherwise = shapedArrFromList outerShape . map (ScalarArr . shapedArrFromList innerShape) . groupsOf groupSz . arrToList $ a
        where outerShape = take n $ shape a
              innerShape = drop n $ shape a
              n = if r >= 0 then (length $ shape a) - r else -1 * r
              groupSz = foldr (*) 1 innerShape

arrReorderAxes :: [Int] -> Array -> Array
arrReorderAxes targetIdxs a
    | length targetIdxs /= (length $ shape a) = undefined
    | otherwise = shapedArrFromList shape' vals'
        where shape' = map (foldr min intMax . map snd) . groupBy (on (==) fst) . sortBy (on compare fst) $ zip targetIdxs (shape a)
              n = foldr (*) 1 shape'
              vals' = map ((a`arrIndex`) . calcIndex) [0..(n - 1)]
              calcIndex i = map ((idxList!!) . (+(-1))) targetIdxs
                  where idxList = calcIndex' i
              calcIndex' i = map (\(e, m) -> i `div` m `mod` e) $ zip shape' indexMod
              indexMod = tail . scanr (*) 1 $ shape'

arithFnD :: (Double -> Double -> Double) -> Array -> Array -> Array
arithFnD f x' y' = arrZipWith (f') x y
    where (x, y) = rankMorph (x', y')
          f' :: Scalar -> Scalar -> Scalar
          f' (ScalarNum n) (ScalarNum m) = ScalarNum $ f n m
          f' n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ rec (listToArr [n]) arr
          f' (ScalarArr a) (ScalarArr b) = ScalarArr $ rec a b
          f' (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ rec arr (listToArr [n])
          f' _ _ = throw $ DomainError "expected number"
          rec = arithFnD f

arithFnM :: (Double -> Double) -> Array -> Array
arithFnM f = arrMap (f')
    where f' (ScalarNum n) = ScalarNum $ f n
          f' (ScalarArr a) = ScalarArr . arithFnM f $ a
          f' _ = throw $ DomainError "expected number"

isScalarArr :: Scalar -> Bool
isScalarArr (ScalarArr _) = True
isScalarArr _ = False

isScalarCh :: Scalar -> Bool
isScalarCh (ScalarCh _) = True
isScalarCh _ = False

intToScalarArr :: Int -> Array
intToScalarArr = listToArr . (:[]) . ScalarNum . fromIntegral

doubleToScalarArr :: Double -> Array
doubleToScalarArr = listToArr . (:[]) . ScalarNum

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

scalarToInt :: Scalar -> Int
scalarToInt (ScalarNum n) | isIntegral n = floor n
scalarToInt _ = throw $ DomainError "expected int singleton"

intToScalar :: Int -> Scalar
intToScalar = ScalarNum . fromIntegral

scalarToDouble :: Scalar -> Double
scalarToDouble (ScalarNum n) = n
scalarToDouble _ = throw $ DomainError "expected numeric singleton"

scalarToChar :: Scalar -> Char
scalarToChar (ScalarCh c) = c
scalarToChar _ = throw $ DomainError "expected character"

scalarToArr :: Scalar -> Array
scalarToArr s = case s of
              ScalarArr a -> a
              _ -> listToArr [s]

scalarToArr_ :: Scalar -> Array
scalarToArr_ = listToArr . (:[])

listToArr_ :: [Scalar] -> Array
listToArr_ (ScalarArr a:[]) = a
listToArr_ l = listToArr l

arrToScalar :: Array -> Scalar
arrToScalar a
    | shape a == [1] = a `at` 0
    | otherwise = ScalarArr a

arrToString :: Array -> String
arrToString a
    | arrRank a /= 1 = throw . RankError $ "expected character scalar or vector"
    | otherwise = map scalarToChar . arrToList $ a
