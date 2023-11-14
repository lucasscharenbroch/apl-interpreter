module Glyphs where
import GrammarTree
import qualified Functions as F
import qualified Operators as O

{- placeholders (TODO remove) -}

mFPH :: String -> ArrTreeNode -> Array
mFPH name _ = arrFromList . map (ScalarCh) $ "result of monadic fn: " ++ name

dFPH :: String -> ArrTreeNode -> ArrTreeNode -> Array
dFPH name _ _ = arrFromList . map (ScalarCh) $ "result of dyadic fn: " ++ name

mOPH :: String -> FnTreeNode -> Function
mOPH name _ = DyadFn ("derived from monadic op: " ++ name) (dFPH "_derived_")

dOPH :: String -> FnTreeNode -> FnTreeNode -> Function
dOPH name _ _ = DyadFn ("derived from dyadic op: " ++ name) (dFPH "_derived_")

{- Functions -}

fImplicitCat = DyadFn ")(" F.implicitCat
fImplicitGroup = MonFn "()" F.implicitGroup
fAssignToQuad = MonFn "⎕←" (mFPH "⎕←")

fReplicate = DyadFn "/" (dFPH "/")
fExpand = DyadFn "\\" (dFPH "\\")
fReplicateFirst = DyadFn "⌿" (dFPH "⌿")
fExpandFirst = DyadFn "⍀" (dFPH "⍀")

fPlus = MonDyadFn "+" F.conjugate F.add
fMinus = MonDyadFn "-" F.negate F.subtract
fIota = MonDyadFn "⍳" F.iota F.indexOf
fShape = MonDyadFn "⍴" F.shapeOf F.reshape

{- Operators -}

oReduce = MonOp "/" (mOPH "/")
oScan = MonOp "\\" (mOPH "\\")
oReduceFirst = MonOp "⌿" (mOPH "⌿")
oScanFirst = MonOp "⍀" (mOPH "⍀")

oSelfie = MonOp "⍨" O.selfie
oAtop = DyadOp "⍤" (dOPH "⍤")

oAxisSpec axis = MonOp ("[" ++ (show axis) ++ "]") (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn
