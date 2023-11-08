module GrammarTree where
import qualified Data.Array as A
import Data.List (intersperse, zip4, elemIndex)

data Scalar = ScalarNum (Either Int Double)
            | ScalarCh Char
            | ScalarArr Array

instance Show Scalar where
    show (ScalarNum (Left i)) = show i
    show (ScalarNum (Right d)) = show d
    show (ScalarCh c) = [c]
    show (ScalarArr a) = show a

data Array = Array {
               shape :: [Int]
             , cells :: A.Array Int Scalar
             }

rowify :: ([[String]], Int) -> [[String]]
rowify (row, height) = foldr (zipWith (:)) (replicate height []) $ row

gridify :: String -> [Int] -> [Int] -> [[[String]]] -> String
gridify delim heights widths cellRows = concat . intersperse "\n" . map (concat . intersperse delim) . concat $ stringRowGroups
    where stringRowGroups = map (rowify) $ zip cellRows heights

boxify :: [Int] -> [Int] -> [[[String]]] -> String
boxify heights widths cellRows = concat . intersperse "\n" $ [topLine] ++ ls ++ [botLine]
    where topLine = "┌" ++ (concat . intersperse "┬" $ (map (flip replicate '─') widths)) ++ "┐"
          midLine = "├" ++ (concat . intersperse "┼" $ (map (flip replicate '─') widths)) ++ "┤"
          botLine = "└" ++ (concat . intersperse "┴" $ (map (flip replicate '─') widths)) ++ "┘"
          ls = concat . intersperse [midLine] $ lsGrouped
          lsGrouped = map (map (\ss -> "│" ++ (concat . intersperse "│" $ ss) ++ "│")) stringRowGroups
          stringRowGroups = map (rowify) $ zip cellRows heights

justify :: Int -> Int -> [String] -> [String] -- height -> width -> lines -> justified-lines
justify height width ls = map (colPad) rowPadded
    where rowPadded = ls ++ (replicate (height - (length ls)) "")
          colPad s = replicate (width - (length s)) ' ' ++ s

leftJustify :: Int -> Int -> [String] -> [String]
leftJustify height width ls = map (colPad) rowPadded
    where rowPadded = ls ++ (replicate (height - (length ls)) "")
          colPad s = s ++ replicate (width - (length s)) ' '

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n x = [take n x] ++ groupBy n (drop n x)

isScalarArr :: Scalar -> Bool
isScalarArr (ScalarArr _) = True
isScalarArr _ = False

isScalarCh :: Scalar -> Bool
isScalarCh (ScalarCh _) = True
isScalarCh _ = False

instance Show Array where
    show (Array [0] _) = ""
    show (Array shape cells) = concat . map (\(h, w, r, n) -> replicate n '\n' ++ showFn h w r) $ tups
        where tups = zip4 (groupBy subgNumRows heights) (replicate (length newlineCnts) widths) (groupBy subgNumRows justifiedCells) newlineCnts
              newlineCnts = 0 : map (\i -> sum . map (fromEnum) . map (\p -> i `mod` p == 0) $ shapeProducts) indicesExcept0
              indicesExcept0 = tail [0,subgSz..(sz - 1)]
              shapeProducts = scanl (*) subgSz (drop 2 shape)
              subgSz = (shape' !! 0) * (shape' !! 1)
              subgNumRows = subgSz `div` (length widths)
              showFn = if any (isScalarArr) $ cellsAsList
                       then boxify
                       else if all (isScalarCh) $ cellsAsList
                            then gridify "" -- print strings without spaces in grid
                            else gridify " "
              cellsAsList = map (cells A.!) [0..(sz - 1)] :: [Scalar]
              heights = map (foldr (max . fst) 0) heightWidth
              widths = foldr (zipWith (\hw mx -> max mx (snd hw))) (replicate numCols 0) heightWidth
              justifiedCells = map (map (\((h, w), s) -> justify h w (lines s))) $ zipWith (zip) justHeightWidth strMatrix
              justHeightWidth = [[(h, w) | w <- widths] | h <- heights]
              heightWidth =  [[getHeightWidthAt (i * numCols + j) | j <- [0..(numCols - 1)]] | i <- [0..(numRows - 1)]]
              strMatrix =  [[show $ cells A.! (i * numCols + j) | j <- [0..(numCols - 1)]] | i <- [0..(numRows - 1)]]
              shape' = if length shape == 1
                       then 1 : shape
                       else shape
              sz = foldr (*) 1 shape
              numCols = shape' !! 1
              numRows = sz `div` numCols
              getHeightWidthAt idx = let ls = lines . show $ cells A.! idx
                                     in (length ls, foldr (max . length) 0 ls)

{- Array Helpers -}

at :: Array -> Int -> Scalar
at a i = (cells a) A.! i

shapedArrFromList :: [Int] -> [Scalar] -> Array
shapedArrFromList shape cells = Array {
                                  shape = shape
                                , cells = A.listArray (0, size - 1) cells
                                }
    where size = foldr (*) 1 shape

arrFromList :: [Scalar] -> Array
arrFromList l = shapedArrFromList [length l] l

arrToList :: Array -> [Scalar]
arrToList (Array shape cells) = [cells A.! i | i <- [0..(sz - 1)]]
    where sz = foldr (*) 1 shape

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

{- Functions and Operators -}

data Function = MonFn String (ArrTreeNode -> Array)
              | DyadFn String (ArrTreeNode -> ArrTreeNode -> Array)
              | MonDyadFn String (ArrTreeNode -> Array) (ArrTreeNode -> ArrTreeNode -> Array)

instance Show Function where
    show (MonFn name _) = name
    show (DyadFn name _)  = name
    show (MonDyadFn name _ _) = name

data Operator = MonOp String (FnTreeNode -> Function)
              | DyadOp String (FnTreeNode -> FnTreeNode -> Function)

instance Show Operator where
    show (MonOp name _) = name
    show (DyadOp name _) = name

{- Tree Nodes -}

-- "function tree": a tree that makes up a derived function:
-- the internal nodes are operators, and the leaves are functions or (derived) arrays
data FnTreeNode = FnLeafFn Function
                | FnLeafArr ArrTreeNode
                | FnInternalMonOp Operator FnTreeNode
                | FnInternalDyadOp Operator FnTreeNode FnTreeNode
                | FnInternalAtop FnTreeNode FnTreeNode
                | FnInternalFork FnTreeNode FnTreeNode FnTreeNode

horizCat :: String -> String -> (String, Int)
horizCat s1 s2 = (res, relOffset)
    where l1 = lines s1
          l2 = lines s2
          relOffset = foldr (max) (minBound :: Int) $ zipWith ((-) . (+1)) (map (length) l1) (map (numLeading ' ') l2)
          numLeading y = length . fst . span (==y)
          l1Augmented = map ((++) (replicate (max 0 (-relOffset)) ' ')) l1
          l2Just = map ((++) (replicate (max 0 relOffset) ' ')) l2
          l2Stripped = zipWith (\a b -> drop (length a)  b) l1' l2Just
          netLines = max (length l1) (length l2)
          l1' = l1Augmented ++ (replicate (netLines - (length l1)) "")
          l2' = l2Stripped ++ (replicate (netLines - (length l2)) "")
          res = (concat $ intersperse "\n" subtrees)
          subtrees = zipWith (++) l1' l2'

showMonTreeHelper :: (String, Int) -> String -> (String, Int)
showMonTreeHelper (child, padSz) node = (res, padSz + 2)
    where res = node' ++ "\n" ++ pad ++ "┌─┘\n" ++ child
          pad = (replicate (padSz) ' ')
          node' = concat . intersperse "\n" . map ((pad ++ "  ") ++) $ lines node

showDyadTreeHelper :: (String, Int) -> (String, Int) -> String -> (String, Int)
showDyadTreeHelper (s1, p1) (s2, p2) node = (res, padSz)
    where (subtrees, relOffset) = horizCat s1 s2
          branchWidth = p2 + (max 0 relOffset) - branchPadSz - 1
          prePipeSpace =  (branchWidth - 1) `div` 2
          branchPadSz = p1 + (max 0 (-relOffset))
          padSz = 1 + prePipeSpace + branchPadSz
          pad = replicate padSz ' '
          branches = "┌" ++ replicate prePipeSpace '─' ++ "┴" ++ replicate (branchWidth - 1 - prePipeSpace) '─' ++ "┐"
          res = node' ++ "\n" ++ replicate branchPadSz ' ' ++ branches ++ "\n" ++ subtrees
          node' = concat . intersperse "\n" . map (pad ++) $ lines node

showFtnHelper :: FnTreeNode -> (String, Int) -- node -> (shown, amount-of-padding)
showFtnHelper (FnLeafFn fn) = (show fn, 0)
showFtnHelper (FnLeafArr arr) = (show arr, 0)
showFtnHelper (FnInternalMonOp op fn) = showMonTreeHelper (showFtnHelper fn) (show op)
showFtnHelper (FnInternalDyadOp op f1 f2) = showDyadTreeHelper (showFtnHelper f1) (showFtnHelper f2) (show op)
showFtnHelper (FnInternalAtop f1 f2) = (concat . intersperse "\n" . tail . lines $ res', padSz')
    where (res', padSz') = showFtnHelper (FnInternalDyadOp dummyOp f1 f2)
          dummyOp = DyadOp "_" (\_ _ -> MonFn "_" (\_ -> arrFromList []))
showFtnHelper (FnInternalFork f1 f2 f3) = (res, padSz)
    where (s1, p1) = showFtnHelper f1
          (s2, p2) = showFtnHelper f2
          (s3, p3) = showFtnHelper f3
          (merge2, offset2) = horizCat s1 s2
          (merge3, offset3) = horizCat merge2 s3
          leftBranchWidth = p2 + (max 0 offset2) + (max 0 (-offset3)) - branchPadSz - 1
          rightBranchWidth = p3 + (max 0 offset3) - branchPadSz - 1 - leftBranchWidth - 1
          branches = "┌" ++ replicate (leftBranchWidth) '─' ++
                     "┼" ++ replicate (rightBranchWidth) '─' ++ "┐"
          branchPadSz = p1 + (max 0 (-offset2)) + (max 0 (-offset3))
          padSz = branchPadSz + leftBranchWidth + 1
          res = replicate branchPadSz ' ' ++ branches ++ "\n" ++ merge3

instance Show FnTreeNode where
    show = fst . showFtnHelper

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeaf Array
                 | ArrInternalSubscript ArrTreeNode [ArrTreeNode]
                 | ArrInternalMonFn FnTreeNode ArrTreeNode
                 | ArrInternalDyadFn FnTreeNode ArrTreeNode ArrTreeNode

singleBoxify :: String -> String
singleBoxify x = boxify [xheight] [xwidth] [[leftJustify xheight xwidth xlines]]
    where xlines = lines x
          xheight = length xlines
          xwidth = foldl (max) 0 $ map (length) xlines

showAtnHelper :: ArrTreeNode -> (String, Int)
showAtnHelper (ArrLeaf a) = (show a, 0)
showAtnHelper (ArrInternalMonFn f a) = showMonTreeHelper (showAtnHelper a) boxedf
    where boxedf = singleBoxify $ show f
showAtnHelper (ArrInternalDyadFn f a1 a2) = showDyadTreeHelper (showAtnHelper a1) (showAtnHelper a2) boxedf
    where boxedf = singleBoxify $ show f
showAtnHelper (ArrInternalSubscript a is) = showDyadTreeHelper (showAtnHelper a) (showIs is, 0) "[]"
    where showIs = foldl (\s a -> fst . horizCat s $ (singleBoxify . show $ a)) ""

instance Show ArrTreeNode where
    show = fst . showAtnHelper
