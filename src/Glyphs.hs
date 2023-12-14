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

pureMonOp :: String -> (Function -> (String -> Function)) -> Operator
pureMonOp name pop = MonOp name ipop
    where ipop idm arg = (idm', pop arg' $ fst $ showMonTreeHelper showA1 name)
            where (idm', arg') = evalFnTree idm arg
                  showA1 = showAndCountPad arg'

pureDyadOp :: String -> (Function -> Function -> (String -> Function)) -> Operator
pureDyadOp name pop = DyadOp name ipop
    where ipop idm arg1 arg2 = (idm'', pop arg1' arg2' $ fst $ showDyadTreeHelper showA1 showA2 name)
            where (idm', arg2') = evalFnTree idm arg2
                  (idm'', arg1') = evalFnTree idm' arg1
                  showA1 = showAndCountPad arg1'
                  showA2 = showAndCountPad arg2'

pureMonOpOptA :: String -> ((Either Function Array) -> (String -> Function)) -> Operator
pureMonOpOptA name pop = MonOp name ipop
    where ipop idm arg = (idm', pop arg' $ fst $ showMonTreeHelper showA1 name)
              where (idm', arg') = case arg of
                                   (FnLeafArr a) -> (\(x, y) -> (x, Right y)) $ evalArrTree idm a
                                   f -> (\(x, y) -> (x, Left y)) $ evalFnTree idm f
                    showA1 = case arg' of
                                    Left f -> showAndCountPad f
                                    Right a -> showAndCountPad a

pureDyadOpOptA :: String -> ((Either Function Array) -> (Either Function Array) -> (String -> Function)) -> Operator
pureDyadOpOptA name pop = DyadOp name ipop
    where ipop idm arg1 arg2 = (idm'', pop arg1' arg2' $ fst $ showDyadTreeHelper showA1 showA2 name)
            where (idm', arg2') = _unwrap idm arg2
                  (idm'', arg1') = _unwrap idm' arg1
                  _unwrap i a = case a of
                                (FnLeafArr arr) -> (\(x, y) -> (x, Right y)) $ evalArrTree i arr
                                f -> (\(x, y) -> (x, Left y)) $ evalFnTree i f
                  showA1 = case arg1' of
                                Left f -> showAndCountPad f
                                Right a -> showAndCountPad a
                  showA2 = case arg2' of
                                Left f -> showAndCountPad f
                                Right a -> showAndCountPad a

{- placeholders (TODO remove) -}

mFPH :: String -> Array -> Array
mFPH name _ = arrFromList . map (ScalarCh) $ "result of monadic fn: " ++ name

dFPH :: String -> Array -> Array -> Array
dFPH name _ _ = arrFromList . map (ScalarCh) $ "result of dyadic fn: " ++ name

mOPH :: String -> Function -> (String -> Function)
mOPH name _ _ = pureDyadFn ("derived from monadic op: " ++ name) (dFPH "_derived_")

dOPH :: String -> Function -> Function -> (String -> Function)
dOPH name _ _ _ = pureDyadFn ("derived from dyadic op: " ++ name) (dFPH "_derived_")

{- Functions -}

-- impure
-- fAssignToId id = MonFn (id ++ "←") (F.assignToId id)

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
fLss = pureDyadFn "<" F.lss
fLeq = pureDyadFn "≤" F.leq
fGtr = pureDyadFn ">" F.gtr
fGeq = pureDyadFn "≥" F.geq
fEqu = pureDyadFn "=" F.equ
fAsterisk = pureMonDyadFn "*" F.exponential F.power
fAsteriskCircle = pureMonDyadFn "⍟" F.naturalLog F.logBase
fFloor = pureMonDyadFn "⌊" F.floor F.minimum
fCeil = pureMonDyadFn "⌈" F.ceiling F.maximum
fRightTack = pureMonDyadFn "⊢" F.identity F.right
fLeftTack = pureMonDyadFn "⊣" F.identity F.left
fPipe = pureMonDyadFn "|" F.absoluteValue F.residue
fTripleEqu = pureMonDyadFn "≡" F.depth F.match
fTripleNeq = pureMonDyadFn "≢" F.tally F.notMatch
fNand = pureDyadFn "⍲" F.nand
fNor = pureDyadFn "⍱" F.nor
fAnd = pureDyadFn "∧" F.lcm
fOr = pureDyadFn "∨" F.gcd
fCircle = pureMonDyadFn "○" F.piTimes F.circularFormulae
fBang = pureMonDyadFn "!" F.factorial F.binomial

functionGlyphs :: [(Char, Function)]
functionGlyphs = [
        ('+', fPlus),
        ('-', fMinus),
        ('×', fTimes),
        ('÷', fDivide),
        ('⍳', fIota),
        ('⍴', fShape),
        ('<', fLss),
        ('≤', fLeq),
        ('>', fGtr),
        ('≥', fGeq),
        ('=', fEqu),
        ('*', fAsterisk),
        ('⍟', fAsteriskCircle),
        ('⌊', fFloor),
        ('⌈', fCeil),
        ('⊢', fRightTack),
        ('⊣', fLeftTack),
        ('|', fPipe),
        ('≡', fTripleEqu),
        ('≢', fTripleNeq),
        ('⍲', fNand),
        ('⍱', fNor),
        ('∧', fAnd),
        ('∨', fOr),
        ('○', fCircle),
        ('!', fBang)
        -- TODO big list of functions
    ]

{- Operators -}

oReduce = pureMonOp "/" (mOPH "/")
oScan = pureMonOp "\\" (mOPH "\\")
oReduceFirst = pureMonOp "⌿" (mOPH "⌿")
oScanFirst = pureMonOp "⍀" (mOPH "⍀")

oSelfie = pureMonOpOptA "⍨" O.selfie
oAtop = pureDyadOp "⍤" (dOPH "⍤")

oAxisSpec axis = pureMonOp ("[" ++ (show axis) ++ "]") (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn

operatorGlyphs :: [(Char, Operator)]
operatorGlyphs = [
        ('⍨', oSelfie),
        ('⍤', oAtop)
        -- TODO big list of operators
    ]
