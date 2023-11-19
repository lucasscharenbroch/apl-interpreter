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

{- Pure Wrappers -}

pureMonFn :: String -> (Array -> Array) -> MonFn
pureMonFn name pfn = MonFn name ipfn
    where ipfn idm arg = (idm', pfn $ arg')
          (idm', arg') = evalArrTree idm arg

pureDyadFn :: String -> (Array -> Array -> Array) -> DyadFn
pureDyadFn name pfn = DyadFn name ipfn
    where ipfn idm arg1 arg2 = (idm'', pfn arg1' arg2')
          (idm', arg2') = evalArrTree idm arg2
          (idm'', arg1') = evalArrTree idm' arg1

pureMonDyadFn :: String -> (Array -> Array) -> (Array -> Array -> Array) -> MonDyadFn
pureMonDyadFn name pmfn pdfn = MonDyadFn name ipmfn ipdfn
    where ipmfn idm arg = (idm', pfn $ arg')
            where (idm', arg') = evalArrTree idm arg
    where ipdfn idm arg1 arg2 = (idm'', pfn arg1' arg2')
            where (idm', arg2') = evalArrTree idm arg2
                  (idm'', arg1') = evalArrTree idm' arg1

{- Functions -}

-- impure
fAssignToId id = MonFn (id ++ "←") (F.assignToId id)

-- specialized
fImplicitCat = pureDyadFn ")(" F.implicitCat
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

oReduce = pureMonOp "/" (mOPH "/")
oScan = pureMonOp "\\" (mOPH "\\")
oReduceFirst = pureMonOp "⌿" (mOPH "⌿")
oScanFirst = pureMonOp "⍀" (mOPH "⍀")

oSelfie = pureMonOp "⍨" O.selfie
oAtop = pureDyadOp "⍤" (dOPH "⍤")

oAxisSpec axis = MonOp ("[" ++ (show axis) ++ "]") (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn
