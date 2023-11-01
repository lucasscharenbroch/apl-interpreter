module GrammarTree where
import qualified Data.Array as A
import Data.List (intersperse)

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

gridify :: [[[String]]] -> String -- convert 2d array of justified multi-line-cells to a grid
gridify = concat . intersperse "\n" . map (concat . intersperse " ") . concat . map (rowify)
    where rowify row = foldr (zipWith (:)) (replicate height []) $ row
              where height = length . head $ row -- assume all cells have same height (justified)

boxify :: [Int] -> [Int] -> [[[String]]] -> String
boxify heights widths cellRows = concat . intersperse "\n" $ [topLine] ++ ls ++ [botLine]
    where topLine = "┌" ++ (concat . intersperse "┬" $ (map (flip replicate '─') widths)) ++ "┐"
          midLine = "├" ++ (concat . intersperse "┼" $ (map (flip replicate '─') widths)) ++ "┤"
          botLine = "└" ++ (concat . intersperse "┴" $ (map (flip replicate '─') widths)) ++ "┘"
          ls = concat . intersperse [midLine] $ lsGrouped
          lsGrouped = map (map (\ss -> "│" ++ (concat . intersperse "│" $ ss) ++ "│")) stringRowGroups
          stringRowGroups = map (rowify) $ zip cellRows heights
          rowify (row, height) = foldr (zipWith (:)) (replicate height []) $ row

justify :: Int -> Int -> [String] -> [String] -- height -> width -> lines -> justified-lines
justify height width ls = map (colPad) rowPadded
    where rowPadded = ls ++ (replicate (height - (length ls)) "")
          colPad s = replicate (width - (length s)) ' ' ++ s

-- TODO if one is boxed, box the whole thing
-- TODO if all are chars, print without spaces
instance Show Array where
    show (Array shape cells) = "\n" ++ (boxify heights widths justifiedCells)
        where heights = map (foldr (max . fst) 0) heightWidth
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
              numRows = numCols `div` sz
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

{- Functions and Operators -}

data Function = MonFn String (ArrTreeNode -> Array)
              | DyadFn String (ArrTreeNode -> ArrTreeNode -> Array)
              | MonDyadFn String (ArrTreeNode -> Array) (ArrTreeNode -> ArrTreeNode -> Array)

instance Show Function where -- TODO remove (debug)
    show (MonFn name _) = name
    show (DyadFn name _)  = name
    show (MonDyadFn name _ _) = name

type OpArg = Either ArrTreeNode FnTreeNode

data Operator = MonOp String (OpArg -> Function)
              | DyadOp String (OpArg -> OpArg -> Function)

instance Show Operator where -- TODO remove (debug)
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
data ArrTreeNode = ArrLeaf Array
                 | ArrInternalMonFn FnTreeNode ArrTreeNode
                 | ArrInternalDyadFn FnTreeNode ArrTreeNode ArrTreeNode
    deriving (Show) -- TODO remove (debug)
