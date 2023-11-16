module Functions where
import Eval
import GrammarTree
import Data.List (elemIndex)

{- Constants -}

intMax = maxBound :: Int
iO = 1 :: Int -- index origin

{- Helpers -}

rankMorph :: ((Array, Array) -> (Array, Array))
rankMorph (x, y)
    | shape x == shape y = (x, y)
    | shape x == [1] = (shapedArrFromList (shape y) xs, y)
    | shape y == [1] = (x, shapedArrFromList (shape x) ys)
    | otherwise = undefined -- TODO throw rank/length error
        where xs = replicate (foldr (*) 1 (shape y)) (at x 0)
              ys = replicate (foldr (*) 1 (shape x)) (at y 0)

toIntVec :: Array -> [Int]
toIntVec = map (toInt) . arrToList
    where toInt (ScalarNum (Left i)) = i
          toInt _ = undefined -- TODO exception

alongAxis :: Array -> Int -> [Array]
alongAxis a ax
    | ax - iO >= (length . shape $ a) = undefined -- TODO throw rank error: invalid axis
    | (length . shape $ a) == 0 = []
    | foldr (*) 1 (shape a) == 0 = []
    | otherwise = map (subarrayAt) [0..(n - 1)]
    where n = (shape a) !! (ax - iO)
          shape' = if (length . shape $ a) == 1
                   then [1]
                   else take (ax - iO) (shape a) ++ drop (ax - iO + 1) (shape a)
          sz = foldr (*) 1 (shape a)
          subarrayAt i = case map (atl a) $ indicesAt i of
              ((ScalarArr a):[]) -> a
              l -> shapedArrFromList shape' l
          indicesAt i =  map (\is -> take (ax - iO) is ++ [i] ++ drop (ax - iO) is) $ map (calcIndex) [0..(sz `div` n - 1)]
          indexMod = tail . reverse $ scanl (*) 1 (reverse shape')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip shape' indexMod

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

arithFn :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> ArrTreeNode -> ArrTreeNode -> Array
arithFn fi' fd' x' y'= arrZipWith (f) x y
    where (x, y) = rankMorph (evalArrTree x', evalArrTree y')
          f :: Scalar -> Scalar -> Scalar
          f (ScalarNum (Left n)) (ScalarNum (Left m)) = ScalarNum . Left $ n `fi'` m
          f (ScalarNum (Left n)) (ScalarNum (Right m)) = ScalarNum . Right $ (fromIntegral n) `fd'` m
          f (ScalarNum (Right n)) (ScalarNum (Left m)) = ScalarNum . Right $ n `fd'` (fromIntegral m)
          f (ScalarNum (Right n)) (ScalarNum (Right m)) = ScalarNum . Right $ n `fd'` m
          f n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ rec (ArrLeaf $ arrFromList [n]) (ArrLeaf arr)
          f (ScalarArr a1) (ScalarArr a2) = ScalarArr $ rec (ArrLeaf a1) (ArrLeaf a2)
          f (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ rec (ArrLeaf arr) (ArrLeaf $ arrFromList [n])
          f _ _ = undefined -- TODO domain error
          rec = arithFn fi' fd'

{- Specialized Functions (non-primitive) -}

implicitCat :: ArrTreeNode -> ArrTreeNode -> Array
implicitCat x' y' = arrCat x y
    where x = case x' of
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> arrFromList [maybeEnclose $ evalArrTree x']
              otherwise -> evalArrTree x'
          y = case y' of
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> arrFromList [maybeEnclose $ evalArrTree y']
              otherwise -> evalArrTree y'
          maybeEnclose arr = case arrToList arr of
                             (s:[]) -> s
                             _ -> ScalarArr arr

implicitGroup :: ArrTreeNode -> Array
implicitGroup = evalArrTree

{- General Functions -}

add :: ArrTreeNode -> ArrTreeNode -> Array
add = arithFn (+) (+)

conjugate :: ArrTreeNode -> Array
conjugate = evalArrTree

direction :: ArrTreeNode -> Array
direction x' = arrMap (_direction) x
    where x = evalArrTree x'
          _direction (ScalarNum (Left i))
              | i == 0 = ScalarNum . Left $ 0
              | i > 0 = ScalarNum . Left $ 1
              | otherwise = ScalarNum . Left  $ -1
          _direction (ScalarNum (Right d))
              | d == 0 = ScalarNum . Left $ 0
              | d >= 0 = ScalarNum . Left $ 1
              | otherwise = ScalarNum . Left  $ -1
          _direction (ScalarArr a) = ScalarArr $ arrMap (_direction) a
          _direction _ = undefined -- TODO dimain error

divide :: ArrTreeNode -> ArrTreeNode -> Array
divide x' y' = arrZipWith (_div) x y
    where (x, y) = rankMorph (evalArrTree x', evalArrTree y')
          _div (ScalarNum n') (ScalarNum m')
              | m == 0 = undefined -- TODO domain error: divide by zero
              | otherwise = ScalarNum . Right $ n / m
           where n = case n' of
                     (Left i) -> fromIntegral i
                     (Right d) -> d
                 m = case m' of
                     (Left i) -> fromIntegral i
                     (Right d) -> d
          _div n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ divide (ArrLeaf $ arrFromList [n]) (ArrLeaf arr)
          _div (ScalarArr a1) (ScalarArr a2) = ScalarArr $ divide (ArrLeaf a1) (ArrLeaf a2)
          _div (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ divide (ArrLeaf arr) (ArrLeaf $ arrFromList [n])
          _div _ _ = undefined -- TODO domain error

iota :: ArrTreeNode -> Array
iota x = shapedArrFromList x' [toScalar . map (ScalarNum . Left . (+iO)) . calcIndex $ i | i <- [0..(sz - 1)]]
    where x' = toIntVec $ evalArrTree x
          sz = foldr (*) 1 x'
          indexMod = reverse . init $ scanl (*) 1 (reverse x')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip x' indexMod
          toScalar (s:[]) = s
          toScalar (ss) = ScalarArr . arrFromList $ ss

indexOf :: ArrTreeNode -> ArrTreeNode -> Array
indexOf x' y'
    | xRank > yRank = undefined -- TODO throw rank error
    | xRank == 1 && (head . shape $ x) <= 1 = undefined -- TODO throw rank error
    | (tail . shape $ x) /= (drop (1 + yRank - xRank) . shape $ y) = undefined -- TODO throw length error
    | otherwise = arrMap (findIndexInXs) ys
    where x = evalArrTree x'
          y = evalArrTree y'
          xRank = length . shape $ x
          yRank = length . shape $ y
          xs = alongAxis x 1
          ys = alongRank y (xRank - 1)
          findIndexInXs e = case elemIndex (toArray e) xs of
              Nothing -> ScalarNum . Left $ iO + (head . shape $ x)
              Just i -> ScalarNum . Left $ iO + i
          toArray (ScalarArr a) = a
          toArray s = arrFromList [s]

multiply :: ArrTreeNode -> ArrTreeNode -> Array
multiply = arithFn (*) (*)

negate :: ArrTreeNode -> Array
negate x' = arrMap (_negate) x
    where x = evalArrTree x'
          _negate (ScalarNum (Left i)) = ScalarNum . Left $ -i
          _negate (ScalarNum (Right d)) = ScalarNum . Right $ -d
          _negate (ScalarArr a) = ScalarArr $ arrMap (_negate) a
          _negate _ = undefined -- TODO domain error

reciprocal :: ArrTreeNode -> Array
reciprocal x' = arrMap (_reciprocal) x
    where x = evalArrTree x'
          _reciprocal (ScalarNum (Left i))
              | i == 0 = undefined -- TODO domain error: divide by zero
              | otherwise = ScalarNum . Right $ 1.0 / (fromIntegral i)
          _reciprocal (ScalarNum (Right d))
              | d == 0 = undefined -- TODO domain error: divide by zero
              | otherwise = ScalarNum . Right $ 1.0 / d
          _reciprocal (ScalarArr a) = ScalarArr $ arrMap (_reciprocal) a
          _reciprocal _ = undefined -- TODO domain error

reshape :: ArrTreeNode -> ArrTreeNode -> Array
reshape x y = shapedArrFromList newShape . take newSize . concat . replicate intMax $ baseList
    where newShape = toIntVec $ evalArrTree x
          newSize = foldr (*) 1 newShape
          baseList = case arrToList . evalArrTree $ y of
                     [] -> [ScalarNum (Left 0)]
                     ss -> ss

shapeOf :: ArrTreeNode -> Array
shapeOf x = arrFromList . map (ScalarNum . Left) . shape . evalArrTree $ x

subtract :: ArrTreeNode -> ArrTreeNode -> Array
subtract = arithFn (-) (-)
