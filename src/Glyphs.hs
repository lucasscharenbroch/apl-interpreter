module Glyphs where
import GrammarTree
import qualified Functions as F
import qualified Operators as O
import Eval

{- Pure Wrappers -}

pureMonFn :: String -> (Array -> Array) -> Function
pureMonFn name pfn = MonFn name ipfn
    where ipfn idm arg = (idm', pfn $ arg')
            where (idm', arg') = evalArrTree idm arg

pureDyadFn :: String -> (Array -> Array -> Array) -> Function
pureDyadFn name pfn = DyadFn name ipfn
    where ipfn idm arg1 arg2 = (idm'', pfn arg1' arg2')
            where (idm', arg2') = evalArrTree idm arg2
                  (idm'', arg1') = evalArrTree idm' arg1

pureMonDyadFn :: String -> (Array -> Array) -> (Array -> Array -> Array) -> Function
pureMonDyadFn name pmfn pdfn = MonDyadFn name ipmfn ipdfn
    where ipmfn idm arg = (idm', pmfn arg')
            where (idm', arg') = evalArrTree idm arg
          ipdfn idm arg1 arg2 = (idm'', pdfn arg1' arg2')
            where (idm', arg2') = evalArrTree idm arg2
                  (idm'', arg1') = evalArrTree idm' arg1

{- placeholders (TODO remove) -}

mFPH :: String -> Array -> Array
mFPH name _ = arrFromList . map (ScalarCh) $ "result of monadic fn: " ++ name

dFPH :: String -> Array -> Array -> Array
dFPH name _ _ = arrFromList . map (ScalarCh) $ "result of dyadic fn: " ++ name

mOPH :: String -> FnTreeNode -> Function
mOPH name _ = pureDyadFn ("derived from monadic op: " ++ name) (dFPH "_derived_")

dOPH :: String -> FnTreeNode -> FnTreeNode -> Function
dOPH name _ _ = pureDyadFn ("derived from dyadic op: " ++ name) (dFPH "_derived_")

{- Functions -}

-- impure
fAssignToId id = MonFn (id ++ "←") (F.assignToId id)

-- specialized
fImplicitCat = DyadFn ")(" F.implicitCat
fImplicitGroup = pureMonFn "()" F.implicitGroup
fAssignToQuad = pureMonFn "⎕←" F.assignToQuad

-- double-as-operators
fReplicate = pureDyadFn "/" (dFPH "/")
fExpand = pureDyadFn "\\" (dFPH "\\")
fReplicateFirst = pureDyadFn "⌿" (dFPH "⌿")
fExpandFirst = pureDyadFn "⍀" (dFPH "⍀")

-- primitive
fPlus = pureMonDyadFn "+" F.conjugate F.add
fMinus = pureMonDyadFn "-" F.negate F.subtract
fTimes = pureMonDyadFn "×" F.direction F.multiply
fDivide = pureMonDyadFn "÷" F.reciprocal F.divide
fIota = pureMonDyadFn "⍳" F.iota F.indexOf
fShape = pureMonDyadFn "⍴" F.shapeOf F.reshape

{- Operators -}

oReduce = MonOp "/" (mOPH "/")
oScan = MonOp "\\" (mOPH "\\")
oReduceFirst = MonOp "⌿" (mOPH "⌿")
oScanFirst = MonOp "⍀" (mOPH "⍀")

oSelfie = MonOp "⍨" O.selfie
oAtop = DyadOp "⍤" (dOPH "⍤")

oAxisSpec axis = MonOp ("[" ++ (show axis) ++ "]") (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn
