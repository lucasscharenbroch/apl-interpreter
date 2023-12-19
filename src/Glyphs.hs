module Glyphs where
import GrammarTree
import qualified Functions as F
import qualified Operators as O
import Eval
import Data.Bifunctor (bimap)

{- Show Helpers -}

_hackShowTreeM :: (Show a) => a -> String -> String
_hackShowTreeM x hs = hackShowTreeM (show x) hs

_hackShowTreeD :: (Show a, Show b) => a -> b -> String -> String
_hackShowTreeD x y hs = hackShowTreeD (show x) (show y) hs

{- Pure Wrappers -}

pureMonFn :: String -> (Array -> Array) -> Function
pureMonFn name f = MonFn name (\a -> return $ f a)

pureDyadFn :: String -> (Array -> Array -> Array) -> Function
pureDyadFn name f = DyadFn name (\a b -> return $ f a b)

pureMonDyadFn :: String -> (Array -> Array) -> (Array -> Array -> Array) -> Function
pureMonDyadFn name fm fd = MonDyadFn name (\a -> return $ fm a) (\a b -> return $ fd a b)

pureMonOp :: String -> (Function -> String -> Function) -> Operator
pureMonOp name o = MonOp name opm
    where opm arg = let arg' = expectFunc arg
                    in return $ o arg' (_hackShowTreeM arg' name)

pureDyadOp :: String -> (Function -> Function -> String -> Function) -> Operator
pureDyadOp name o = DyadOp name opd
    where opd arg1 arg2 = let arg1' = expectFunc arg1
                              arg2' = expectFunc arg2
                          in return $ o arg1' arg2' (_hackShowTreeD arg1' arg2' name)

pureMonOpOptA :: String -> ((Either Array Function) -> (String -> Function)) -> Operator
pureMonOpOptA name o = MonOp name opm
    where opm arg = return $ o arg (hackShowTreeM (liftHomoEither . bimap show show $ arg) name)

pureDyadOpOptA :: String -> ((Either Array Function) -> (Either Array Function) -> (String -> Function)) -> Operator
pureDyadOpOptA name o = DyadOp name opd
    where opd arg1 arg2 = let _showSubtree = liftHomoEither . bimap show show
                          in return $ o arg1 arg2 (_hackShowTreeD (_showSubtree arg1) (_showSubtree arg2) name)

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
fImplicitGroup = pureMonFn "()" F.implicitGroup

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

opOrFnGlyphs :: [(Char, Operator, Function)]
opOrFnGlyphs = [
        ('/', oReduce, fReplicate),
        ('⌿', oReduceFirst, fReplicateFirst),
        ('\\', oScan, fExpand),
        ('⍀', oScanFirst, fExpandFirst)
    ]
