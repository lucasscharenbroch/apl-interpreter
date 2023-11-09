module Functions where
import Eval
import GrammarTree

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

{- Specialized Functions (non-primitive) -}

implicitCat :: ArrTreeNode -> ArrTreeNode -> Array
implicitCat x' y' = arrCat x y
    where x = case x' of --
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
add x' y'= arrZipWith (plus) x y
    where (x, y) = rankMorph (evalArrTree x', evalArrTree y')
          plus (ScalarNum (Left n)) (ScalarNum (Left m)) = ScalarNum . Left $ n + m
          plus (ScalarNum (Left n)) (ScalarNum (Right m)) = ScalarNum . Right $ (fromIntegral n) + m
          plus (ScalarNum (Right n)) (ScalarNum (Left m)) = ScalarNum . Right $ n + (fromIntegral m)
          plus (ScalarNum (Right n)) (ScalarNum (Right m)) = ScalarNum . Right $ n + m
          plus n@(ScalarNum _) (ScalarArr arr) = ScalarArr $ add (ArrLeaf $ arrFromList [n]) (ArrLeaf arr)
          plus (ScalarArr a1) (ScalarArr a2) = ScalarArr $ add (ArrLeaf a1) (ArrLeaf a2)
          plus (ScalarArr arr) n@(ScalarNum _) = ScalarArr $ add (ArrLeaf arr) (ArrLeaf $ arrFromList [n])
          plus _ _ = undefined -- TODO domain error

conjugate :: ArrTreeNode -> Array
conjugate = evalArrTree

iota :: ArrTreeNode -> Array
iota _ = arrFromList []

indexOf :: ArrTreeNode -> ArrTreeNode -> Array
indexOf _ _ = arrFromList []

reshape :: ArrTreeNode -> ArrTreeNode -> Array
reshape x y = shapedArrFromList newShape . take newSize . concat . replicate intMax $ baseList
    where newShape = toIntVec $ evalArrTree x
          newSize = foldr (*) 1 newShape
          baseList = case arrToList . evalArrTree $ y of
                     [] -> [ScalarNum (Left 0)]
                     ss -> ss

shapeOf :: ArrTreeNode -> Array
shapeOf x = arrFromList . map (ScalarNum . Left) . shape . evalArrTree $ x
