module Functions where
import Eval
import GrammarTree
import Data.List (elemIndex)

{- Constants -}

intMax = maxBound :: Int
iO = 1 :: Int -- index origin

{- Helpers -}

rankMorph :: (Array, Array) -> (Array, Array)
rankMorph (x, y)
    | shape x == shape y = (x, y)
    | shape x == [1] = (shapedArrFromList (shape y) xs, y)
    | shape y == [1] = (x, shapedArrFromList (shape x) ys)
    | otherwise = undefined -- TODO throw rank/length error
        where xs = replicate (foldr (*) 1 (shape y)) (at x 0)
              ys = replicate (foldr (*) 1 (shape x)) (at y 0)

toIntVec :: Array -> [Int]
toIntVec = map (toInt) . arrToList
    where toInt (ScalarNum n) | (fromIntegral . Prelude.floor $ n) - n == 0 = Prelude.floor n
          toInt _ = undefined -- TODO exception (domain error)

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

arithFnD :: (Double -> Double -> Double) -> Array -> Array -> Array
arithFnD f x' y' = arrZipWith (f') x y
    where (x, y) = rankMorph (x', y')
          f' :: Scalar -> Scalar -> Scalar
          f' (ScalarNum n) (ScalarNum m) = ScalarNum $ f n m
          f' n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ rec (arrFromList [n]) arr
          f' (ScalarArr a) (ScalarArr b) = ScalarArr $ rec a b
          f' (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ rec arr (arrFromList [n])
          f' _ _ = undefined -- TODO domain error
          rec = arithFnD f

arithFnM :: (Double -> Double) -> Array -> Array
arithFnM f = arrMap (f')
    where f' (ScalarNum n) = ScalarNum $ f n
          f' (ScalarArr a) = ScalarArr . arithFnM f $ a
          f' _ = undefined -- TODO domain error

{- Impure Functions -}

{-
assignToId :: String -> FuncM
assignToId id idm x = (mapInsert id (IdArr x') idm', x')
    where (idm', x') = evalArrTree idm x
-}

{- Specialized Functions (non-primitive) -}

assignToQuad :: Array -> Array
assignToQuad = id

implicitCat :: FuncD
implicitCat idm x y = (idm'', arrCat x' y')
    where (idm'', x') = case x of
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> (_idm, arrFromList [maybeEnclose _x])
                  where (_idm, _x) = evalArrTree idm' x
              otherwise -> evalArrTree idm' x
          (idm', y') = case y of
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> (_idm, arrFromList [maybeEnclose _y])
                  where (_idm, _y) = evalArrTree idm y
              otherwise -> evalArrTree idm y
          maybeEnclose arr = case arrToList arr of
                             (s:[]) -> s
                             _ -> ScalarArr arr

implicitGroup :: Array -> Array
implicitGroup = id

{- General Functions -}

add :: Array -> Array -> Array
add = arithFnD (+)

ceiling :: Array -> Array
ceiling = arithFnM (fromIntegral. Prelude.ceiling)

conjugate :: Array -> Array
conjugate = id

direction :: Array -> Array
direction = arithFnM _direction
    where _direction n
              | n < 0 = -1
              | n == 0 = 0
              | n > 0 = 1

divide :: Array -> Array -> Array
divide = arithFnD _divide
    where _divide _ 0 = undefined -- TODO domain error
          _divide n m = n / m

equ :: Array -> Array -> Array
equ = arithFnD (\n m -> fromIntegral . fromEnum $ n == m)

exponential :: Array -> Array
exponential = arithFnM (exp)

floor :: Array -> Array
floor = arithFnM (fromIntegral . Prelude.floor)

geq :: Array -> Array -> Array
geq = arithFnD (\n m -> fromIntegral . fromEnum $ n >= m)

gtr :: Array -> Array -> Array
gtr = arithFnD (\n m -> fromIntegral . fromEnum $ n > m)

identity :: Array -> Array
identity = id

iota :: Array -> Array
iota x = shapedArrFromList x' [toScalar . map (ScalarNum . fromIntegral . (+iO)) . calcIndex $ i | i <- [0..(sz - 1)]]
    where x' = toIntVec x
          sz = foldr (*) 1 x'
          indexMod = reverse . init $ scanl (*) 1 (reverse x')
          calcIndex i = map (\(e, m) -> i `div` m `mod` e) $ zip x' indexMod
          toScalar (s:[]) = s
          toScalar (ss) = ScalarArr . arrFromList $ ss

indexOf :: Array -> Array -> Array
indexOf x y
    | xRank > yRank = undefined -- TODO throw rank error
    | xRank == 1 && (head . shape $ x) <= 1 = undefined -- TODO throw rank error
    | (tail . shape $ x) /= (drop (1 + yRank - xRank) . shape $ y) = undefined -- TODO throw length error
    | otherwise = arrMap (findIndexInXs) ys
    where xRank = length . shape $ x
          yRank = length . shape $ y
          xs = alongAxis x 1
          ys = alongRank y (xRank - 1)
          findIndexInXs e = case elemIndex (toArray e) xs of
              Nothing -> ScalarNum . fromIntegral $ iO + (head . shape $ x)
              Just i -> ScalarNum . fromIntegral $ iO + i
          toArray (ScalarArr a) = a
          toArray s = arrFromList [s]

left :: Array -> Array -> Array
left = const

leq :: Array -> Array -> Array
leq = arithFnD (\n m -> fromIntegral . fromEnum $ n <= m)

logBase :: Array -> Array -> Array
logBase = arithFnD (_logBase)
    where _logBase b n | b <= 0 || b == 1 || n <= 0 = undefined -- TODO domain error
          _logBase b n = Prelude.logBase b n

lss :: Array -> Array -> Array
lss = arithFnD (\n m -> fromIntegral . fromEnum $ n < m)

minimum :: Array -> Array -> Array
minimum = arithFnD (min)

maximum :: Array -> Array -> Array
maximum = arithFnD (max)

multiply :: Array -> Array -> Array
multiply = arithFnD (*)

naturalLog :: Array -> Array
naturalLog = arithFnM (_log)
    where _log n | n <= 0 = undefined -- TODO domain error
          _log n = log n

negate :: Array -> Array
negate = arithFnM (Prelude.negate)

power :: Array -> Array -> Array
power = arithFnD (**)

reciprocal :: Array -> Array
reciprocal = arithFnM (_reciprocal)
    where _reciprocal 0 = undefined -- TODO domain error: divide by zero
          _reciprocal n = 1 / n

reshape :: Array -> Array -> Array
reshape x y = shapedArrFromList newShape . take newSize . concat . replicate intMax $ baseList
    where newShape = toIntVec x
          newSize = foldr (*) 1 newShape
          baseList = case arrToList y of
                     [] -> [ScalarNum 0]
                     ss -> ss
right :: Array -> Array -> Array
right = flip const

shapeOf :: Array -> Array
shapeOf = arrFromList . map (ScalarNum . fromIntegral) . shape

subtract :: Array -> Array -> Array
subtract = arithFnD (-)
