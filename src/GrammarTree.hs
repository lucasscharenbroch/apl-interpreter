module GrammarTree where
import qualified Data.Array as A
import Data.List (intersperse, zip4)

data Scalar = ScalarNum (Either Int Double)
            | ScalarCh Char
            | ScalarArr ArrTreeNode

instance Show Scalar where
    show (ScalarNum (Left i)) = show i
    show (ScalarNum (Right d)) = show d
    show (ScalarCh c) = [c]
    show (ScalarArr atn) = show atn -- TODO evaluate then show

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

{- Functions and Operators -}

data Function = MonFn String (ArrTreeNode -> Array)
              | DyadFn String (ArrTreeNode -> ArrTreeNode -> Array)
              | MonDyadFn String (ArrTreeNode -> Array) (ArrTreeNode -> ArrTreeNode -> Array)

instance Show Function where
    show (MonFn name _) = name
    show (DyadFn name _)  = name
    show (MonDyadFn name _ _) = name

type OpArg = Either ArrTreeNode FnTreeNode

data Operator = MonOp String (OpArg -> Function)
              | DyadOp String (OpArg -> OpArg -> Function)

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
    deriving (Show) -- TODO remove (debug)

-- "array tree": a tree that makes up a derived array
data ArrTreeNode = ArrLeafArr Array
                 | ArrLeafSc Scalar
                 | ArrInternalMonFn FnTreeNode ArrTreeNode
                 | ArrInternalDyadFn FnTreeNode ArrTreeNode ArrTreeNode

instance Show ArrTreeNode where
    show (ArrLeafArr a) = show a
    show (ArrLeafSc s) = show s
    show (ArrInternalMonFn f r) = show f ++ "(" ++ show r ++ ")"
    show (ArrInternalDyadFn f l r) = "(" ++ show l ++ ")" ++ show f ++ "(" ++ show r ++ ")"
