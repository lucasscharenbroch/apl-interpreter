module Glyphs where
import GrammarTree
import qualified Functions as F

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

{- TODO do this in unwrapScalar function
where scalarify (ArrLeaf a@(Array [1] _)) = a `at` 0 -- don't box scalar
      scalarify a = ScalarArr a
-}


fReplicate = DyadFn "/" (dFPH "/")
fExpand = DyadFn "\\" (dFPH "\\")
fReplicateFirst = DyadFn "⌿" (dFPH "⌿")
fExpandFirst = DyadFn "⍀" (dFPH "⍀")

fPlus = MonDyadFn "+" (mFPH "+") (dFPH "+")
fIota = MonDyadFn "⍳" (mFPH "⍳") (dFPH "⍳")

{- Operators -}

oReduce = MonOp "/" (mOPH "/")
oScan = MonOp "\\" (mOPH "\\")
oReduceFirst = MonOp "⌿" (mOPH "⌿")
oScanFirst = MonOp "⍀" (mOPH "⍀")

oSelfie = MonOp "⍨" (mOPH "⍨")
oAtop = DyadOp "⍤" (dOPH "⍤")

oAxisSpec axis = MonOp "[]" (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn
