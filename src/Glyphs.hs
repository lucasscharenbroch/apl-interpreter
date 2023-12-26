module Glyphs where
import GrammarTree
import qualified Functions as F
import Functions (SubEvalM(..), IdxOriginM)
import qualified Operators as O
import Eval
import Data.Bifunctor (bimap)
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Control.Monad.Reader

{- Show Helpers -}

_hackShowTreeM :: (Show a) => a -> String -> String
_hackShowTreeM x hs = hackShowTreeM (show x) hs

_hackShowTreeD :: (Show a, Show b) => a -> b -> String -> String
_hackShowTreeD x y hs = hackShowTreeD (show x) (show y) hs

{- Monad Wrappers -}

mkMonFn :: SubEvalM m => String -> (Array -> m Array) -> Function
mkMonFn name f = MonFn name (\a -> toEvalM $ f a)

mkDyadFn :: SubEvalM m => String -> (Array -> Array -> m Array) -> Function
mkDyadFn name f = DyadFn name (\a b -> toEvalM $ f a b)

mkMonDyadFn :: (SubEvalM m0, SubEvalM m1) => String -> (Array -> m0 Array) -> (Array -> Array -> m1 Array) -> Function
mkMonDyadFn name fm fd = MonDyadFn name (\a -> toEvalM $ fm a) (\a b -> toEvalM $ fd a b)

mkMonOp :: SubEvalM m => String -> (Function -> String -> m Function) -> Operator
mkMonOp name o = MonOp name opm
  where opm arg = let arg' = expectFunc arg
                  in toEvalM $ o arg' (_hackShowTreeM arg' name)

mkDyadOp :: SubEvalM m => String -> (Function -> Function -> String -> m Function) -> Operator
mkDyadOp name o = DyadOp name opd
  where opd arg1 arg2 = let arg1' = expectFunc arg1
                            arg2' = expectFunc arg2
                        in toEvalM $ o arg1' arg2' (_hackShowTreeD arg1' arg2' name)

mkMonOpOptA :: SubEvalM m => String -> ((Either Array Function) -> (String -> m Function)) -> Operator
mkMonOpOptA name o = MonOp name opm
  where opm arg = toEvalM $ o arg (hackShowTreeM (fromHomoEither . bimap show show $ arg) name)

mkDyadOpOptA :: SubEvalM m => String -> ((Either Array Function) -> (Either Array Function) -> (String -> m Function)) -> Operator
mkDyadOpOptA name o = DyadOp name opd
    where opd arg1 arg2 = let _showSubtree = fromHomoEither . bimap show show
                          in toEvalM $ o arg1 arg2 (_hackShowTreeD (_showSubtree arg1) (_showSubtree arg2) name)

{- Pure Wrappers -}

pureMonFn :: String -> (Array -> Array) -> Function
pureMonFn name f = mkMonFn name (Identity . f)

pureDyadFn :: String -> (Array -> Array -> Array) -> Function
pureDyadFn name f = mkDyadFn name (curry $ Identity . uncurry f)

pureMonDyadFn :: String -> (Array -> Array) -> (Array -> Array -> Array) -> Function
pureMonDyadFn name fm fd = mkMonDyadFn name (Identity . fm) (curry $ Identity . uncurry fd)

pureMonOp :: String -> (Function -> String -> Function) -> Operator
pureMonOp name o = mkMonOp name (curry $ Identity . uncurry o)

pureDyadOp :: String -> (Function -> Function -> String -> Function) -> Operator
pureDyadOp name o = mkDyadOp name (curry . curry $ Identity . uncurry (uncurry o))

pureMonOpOptA :: String -> ((Either Array Function) -> (String -> Function)) -> Operator
pureMonOpOptA name o = mkMonOpOptA name (curry $ Identity . uncurry o)

pureDyadOpOptA :: String -> ((Either Array Function) -> (Either Array Function) -> (String -> Function)) -> Operator
pureDyadOpOptA name o = mkDyadOpOptA name (curry . curry $ Identity . uncurry (uncurry o))

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

-- specialized
fImplicitGroup = pureMonFn "()" F.implicitGroup
fGetString = F.getString

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
fIota = mkMonDyadFn "⍳" F.iota F.indexOf
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
fQuestion = mkMonDyadFn "?" F.roll F.deal

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
        ('!', fBang),
        ('?', fQuestion)
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
