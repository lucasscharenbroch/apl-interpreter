module PrettyPrint where
import qualified Data.Array as A
import Lex
import GrammarTree
import Data.List (intersperse, zip4, elemIndex)
import Data.Char (toUpper)
import Util

{- Misc Helpers -}

swapMinus '-' = '¯'
swapMinus c = c

maxDigits = 10

_truncate :: Int -> Double -> Double
_truncate numPlaces d = (/mult) . fromIntegral . floor . (*mult) $ d
    where mult = 10^numPlaces

_round :: Int -> Double -> Double
_round numPlaces d = (/mult) . fromIntegral . round . (*mult) $ d
    where mult = 10^numPlaces

showSigFigures :: Int -> Double -> String
showSigFigures i d'
    | d == 0 = "0"
    | e >= i = sign ++ (show . (/(10^i')) . _round 0 $ d / (10^(e-i'))) ++ "E" ++ (show e)
    | d - (fromIntegral intPortion) == 0 = sign ++ show intPortion
    | d <= (1e-6) = sign ++ (head . show $ decPortionInt) : "." ++ (stripTrailingZeroes . tail . show $ decPortionInt) ++ "E¯" ++ (show (numDecZeroes + 1))
    | otherwise = if e == i' || (length . stripTrailingZeroes . show $ decPortionInt) == 0 -- no decimal portion
                  then sign ++ (show roundedIntPortion)
                  else sign ++ (show intPortion) ++ "." ++ replicate numDecZeroes '0' ++ (stripTrailingZeroes . show $ decPortionInt)
        where e = floor $ (logBase 10 d)
              i' = i - 1
              intPortion = floor d
              roundedIntPortion = round d
              numShownDecPlaces = if d <= 0
                                  then i
                                  else i' - e
              decPortion = _round numShownDecPlaces $ d - fromIntegral intPortion
              decPortionInt = round $ (10^numShownDecPlaces) * decPortion
              stripTrailingZeroes = reverse . dropWhile (=='0') . reverse
              numDecZeroes = max 0 . ((-1)*) . truncate . logBase 10 $ decPortion
              sign = if d' < 0
                     then "¯"
                     else ""
              d = abs d'

showTokListAsDfn :: [Token] -> String
showTokListAsDfn toks = ("{" ++ (filter (/='\n'). concat . intersperse " " . map (showTokVal) $ toks) ++ "}")
    where showTokVal (NumTok n) = show n
          showTokVal (StrTok s) = "'" ++ s ++ "'"
          showTokVal (IdTok s) = s
          showTokVal AATok = "⍺⍺"
          showTokVal WWTok = "⍵⍵"
          showTokVal DDTok = "∇∇"
          showTokVal (ChTok c) = c:[]

{- Scalars -}

instance Show Scalar where
    show (ScalarNum d) =  map (swapMinus) . map (toUpper) . showSigFigures maxDigits $ d
    show (ScalarCh c) = [c]
    show (ScalarArr a) = show a

{- Arrays -}

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

rightJustify :: Int -> Int -> [String] -> [String] -- height -> width -> lines -> justified-lines
rightJustify height width ls = map (colPad) rowPadded
    where rowPadded = ls ++ (replicate (height - (length ls)) "")
          colPad s = replicate (width - (length s)) ' ' ++ s

leftJustify :: Int -> Int -> [String] -> [String]
leftJustify height width ls = map (colPad) rowPadded
    where rowPadded = ls ++ (replicate (height - (length ls)) "")
          colPad s = s ++ replicate (width - (length s)) ' '

instance Show Array where
    show (Array shape _)
        | foldr (*) 1 shape == 0 = ""
        | shape == [] = singleBoxify "\n" -- not the same as zilde
    show (Array shape cells) = concat . map (\(h, w, r, n) -> replicate n '\n' ++ showFn h w r) $ tups
        where tups = zip4 (groupsOf subgNumRows heights) (replicate (length newlineCnts) widths) (groupsOf subgNumRows justifiedCells) newlineCnts
              newlineCnts = 0 : map (\i -> (newlineInc) . sum . map (fromEnum) . map (\p -> i `mod` p == 0) $ shapeProducts) indicesExcept0
              indicesExcept0 = tail [0,subgSz..(sz - 1)]
              shapeProducts = scanl (*) subgSz (drop 2 shape')
              subgSz = (shape' !! 0) * (shape' !! 1)
              subgNumRows = subgSz `div` (length widths)
              (showFn, justifyFn, newlineInc) = if any (isScalarArr) $ cellsAsList
                       then (boxify, leftJustify, id)
                       else if all (isScalarCh) $ cellsAsList
                            then (gridify "", rightJustify, (+1)) -- print strings without spaces in grid
                            else (gridify " ", rightJustify, (+1))
              cellsAsList = map (cells A.!) [0..(sz - 1)] :: [Scalar]
              heights = map (foldr (max . fst) 0) heightWidth
              widths = foldr (zipWith (\hw mx -> max mx (snd hw))) (replicate numCols 0) heightWidth
              justifiedCells = map (map (\((h, w), s) -> justifyFn h w (lines s))) $ zipWith (zip) justHeightWidth strMatrix
              justHeightWidth = [[(h, w) | w <- widths] | h <- heights]
              heightWidth =  [[getHeightWidthAt (i * numCols + j) | j <- [0..(numCols - 1)]] | i <- [0..(numRows - 1)]]
              strMatrix =  [[show $ cells A.! (i * numCols + j) | j <- [0..(numCols - 1)]] | i <- [0..(numRows - 1)]]
              shape' = case reverse shape of
                       (c:[]) -> 1 : [c]
                       (c:r:ds) -> r : c : ds
              sz = foldr (*) 1 shape
              numCols = shape' !! 1
              numRows = sz `div` numCols
              getHeightWidthAt idx = let ls = lines . show $ cells A.! idx
                                     in (length ls, foldr (max . length) 0 ls)

{- Functions -}

instance Show Function where
    show (MonFn info _) = fnNameM info
    show (DyadFn info _)  = fnNameD info
    show (AmbivFn info _ _) = fnNameA info

{- Operators -}

instance Show Operator where
    show (MonOp name _) = name
    show (DyadOp name _) = name

{- Trees -}

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

showAtopHelper :: (String, Int) -> (String, Int) -> (String, Int)
showAtopHelper sp1 sp2 = (concat . intersperse "\n" . tail . lines $ res', padSz')
    where (res', padSz') = showDyadTreeHelper sp1 sp2 ""

showForkHelper :: (String, Int) -> (String, Int) -> (String, Int) -> (String, Int)
showForkHelper (s1, p1) (s2, p2) (s3, p3) = (res, padSz)
    where (merge2, offset2) = horizCat s1 s2
          (merge3, offset3) = horizCat merge2 s3
          leftBranchWidth = p2 + (max 0 offset2) + (max 0 (-offset3)) - branchPadSz - 1
          rightBranchWidth = p3 + (max 0 offset3) - branchPadSz - 1 - leftBranchWidth - 1
          branches = "┌" ++ replicate (leftBranchWidth) '─' ++
                     "┼" ++ replicate (rightBranchWidth) '─' ++ "┐"
          branchPadSz = p1 + (max 0 (-offset2)) + (max 0 (-offset3))
          padSz = branchPadSz + leftBranchWidth + 1
          res = replicate branchPadSz ' ' ++ branches ++ "\n" ++ merge3

showFtnHelper :: FnTreeNode -> (String, Int) -- node -> (shown, amount-of-padding)
showFtnHelper (FnLeafFn fn) = (show fn, 0)
showFtnHelper (FnLeafArr arr) = (show arr, 0)
showFtnHelper (FnLeafVar s) = (s, 0)
showFtnHelper (FnInternalMonOp op fn) = showMonTreeHelper (showFtnHelper fn) (show op)
showFtnHelper (FnInternalDyadOp op f1 f2) = showDyadTreeHelper (showFtnHelper f1) (showFtnHelper f2) (show op)
showFtnHelper (FnInternalAtop f1 f2) = showAtopHelper h1 h2
    where h1 = showFtnHelper f1
          h2 = showFtnHelper f2
showFtnHelper (FnInternalFork f1 f2 f3) = showForkHelper h1 h2 h3
    where h1 = showFtnHelper f1
          h2 = showFtnHelper f2
          h3 = showFtnHelper f3
showFtnHelper (FnInternalAssignment id child) = showMonTreeHelper (showFtnHelper child) (id ++ " ←")
showFtnHelper (FnInternalQuadAssignment child) = showMonTreeHelper (showFtnHelper child) "⎕ ←"
showFtnHelper (FnInternalAxisSpec ftn atn) = showDyadTreeHelper (showFtnHelper ftn) ((singleBoxify $ show atn), 0) "[]"
showFtnHelper (FnInternalDummyNode child) = showFtnHelper child

instance Show FnTreeNode where
    show = fst . showFtnHelper

singleBoxify :: String -> String
singleBoxify x = boxify [xheight] [xwidth] [[leftJustify xheight xwidth xlines]]
    where xlines = lines x
          xheight = length xlines
          xwidth = foldl (max) 0 $ map (length) xlines

showAtnHelper :: ArrTreeNode -> (String, Int)
showAtnHelper (ArrLeaf a) = (show a, 0)
showAtnHelper (ArrLeafVar s) = (s, 0)
showAtnHelper (ArrNiladicFn name _) = (name, 0)
showAtnHelper (ArrInternalMonFn f a) = showMonTreeHelper (showAtnHelper a) boxedf
    where boxedf = singleBoxify $ show f
showAtnHelper (ArrInternalDyadFn f a1 a2) = showDyadTreeHelper (showAtnHelper a1) (showAtnHelper a2) boxedf
    where boxedf = singleBoxify $ show f
showAtnHelper (ArrInternalSubscript a is) = showDyadTreeHelper (showAtnHelper a) (showIs is, 0) "[]"
    where showIs = foldl (\s mba -> fst . horizCat s $ (singleBoxify . showMb $ mba)) ""
          showMb mb = case mb of
              Nothing -> "/"
              Just x -> show x
showAtnHelper (ArrInternalAssignment id a) = showMonTreeHelper (showAtnHelper a) (id ++ " ←")
showAtnHelper (ArrInternalModAssignment id f a) = showMonTreeHelper (showAtnHelper a) (id ++ " " ++ show f ++ "←")
showAtnHelper (ArrInternalSelAssignment l r) = showDyadTreeHelper (showAtnHelper l) (showAtnHelper r) ("←")
showAtnHelper (ArrInternalQuadAssignment a) = showMonTreeHelper (showAtnHelper a) ("⎕ ←")
showAtnHelper (ArrInternalQuadIdAssignment id a) = showMonTreeHelper (showAtnHelper a) ("⎕" ++ id ++ " ←")
showAtnHelper (ArrInternalImplCat a1 a2) = showDyadTreeHelper (showAtnHelper a1) (showAtnHelper a2) (singleBoxify ")(")
showAtnHelper (ArrInternalImplGroup atn) = showMonTreeHelper (showAtnHelper atn) (singleBoxify "()")

instance Show ArrTreeNode where
    show = fst . showAtnHelper

showOtnHelper :: OpTreeNode -> (String, Int)
showOtnHelper (OpLeaf o) = (show o, 0)
showOtnHelper (OpInternalAssignment id next) = showMonTreeHelper (showOtnHelper next) (id ++ " ←")
showOtnHelper (OpInternalQuadAssignment next) = showMonTreeHelper (showOtnHelper next) ("⎕ ←")
showOtnHelper (OpInternalDummyNode next) = showOtnHelper next

instance Show OpTreeNode where
    show = fst . showOtnHelper
