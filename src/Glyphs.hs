module Glyphs where
import GrammarTree

{- placeholders (TODO remove) -}
mFPH :: String -> ArrTreeNode -> Array
mFPH name _ = arrFromList . map (ScalarCh) $ "result of monadic fn: " ++ name

dFPH :: String -> ArrTreeNode -> ArrTreeNode -> Array
dFPH name _ _ = arrFromList . map (ScalarCh) $ "result of dyadic fn: " ++ name

mOPH :: String -> OpArg -> Function
mOPH name _ = DyadFn ("derived from monadic op: " ++ name) (dFPH "_derived_")

dOPH :: String -> OpArg -> OpArg -> Function
dOPH name _ _ = DyadFn ("derived from dyadic op: " ++ name) (dFPH "_derived_")

{- Functions -}
fSubscript = MonFn "[]" (mFPH "[]")

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

oAxisSpec axis = MonOp "[]" (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn
