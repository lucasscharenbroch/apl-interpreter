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
import Data.Function (on)
import Util

{- Function Info Helpers -}

mkFnInfoM :: String -> FnInfoM
mkFnInfoM s = defFnInfoM {fnNameM = s}

mkFnInfoD :: String -> FnInfoD
mkFnInfoD s = defFnInfoD {fnNameD = s}

mkFnInfoA :: String -> FnInfoA
mkFnInfoA s = defFnInfoA {fnNameA = s}

{- Monad Wrappers -}

mkFuncM :: SubEvalM m => (Array -> m Array) -> FuncM
mkFuncM f a = toEvalM $ f a

mkFuncD :: SubEvalM m => (Array -> Array -> m Array) -> FuncD
mkFuncD f a b = toEvalM $ f a b

mkMonFn :: SubEvalM m => FnInfoM -> (Array -> m Array) -> Function
mkMonFn i f = MonFn i (\a -> toEvalM $ f a)

mkDyadFn :: SubEvalM m => FnInfoD -> (Array -> Array -> m Array) -> Function
mkDyadFn i f = DyadFn i (\a b -> toEvalM $ f a b)

mkMonDyadFn :: (SubEvalM m0, SubEvalM m1) => FnInfoA -> (Array -> m0 Array) -> (Array -> Array -> m1 Array) -> Function
mkMonDyadFn ia fm fd = MonDyadFn ia (\a -> toEvalM $ fm a) (\a b -> toEvalM $ fd a b)

mkMonOp :: SubEvalM m => String -> (Function -> m Function) -> Operator
mkMonOp name o = MonOp name (toEvalM . o . expectFunc)

mkDyadOp :: SubEvalM m => String -> (Function -> Function -> m Function) -> Operator
mkDyadOp name o = DyadOp name (toEvalM .: o `on` expectFunc)

mkMonOpOptA :: SubEvalM m => String -> ((Either Array Function) -> m Function) -> Operator
mkMonOpOptA name o = MonOp name (toEvalM . o)

mkDyadOpOptA :: SubEvalM m => String -> ((Either Array Function) -> (Either Array Function) -> m Function) -> Operator
mkDyadOpOptA name o = DyadOp name (toEvalM .: o)

{- Pure Wrappers -}

pureFuncM f = mkFuncM (Identity . f)

pureFuncD f = mkFuncD (Identity .: f)

pureMonFn :: FnInfoM -> (Array -> Array) -> Function
pureMonFn i f = mkMonFn i (Identity . f)

pureDyadFn :: FnInfoD -> (Array -> Array -> Array) -> Function
pureDyadFn i f = mkDyadFn i (Identity .: f)

pureMonDyadFn :: FnInfoA -> (Array -> Array) -> (Array -> Array -> Array) -> Function
pureMonDyadFn ia fm fd = mkMonDyadFn ia (Identity . fm) (Identity .: fd)

pureMonOp :: String -> (Function -> Function) -> Operator
pureMonOp name o = mkMonOp name (Identity . o)

pureDyadOp :: String -> (Function -> Function -> Function) -> Operator
pureDyadOp name o = mkDyadOp name (Identity .: o)

pureMonOpOptA :: String -> ((Either Array Function) -> Function) -> Operator
pureMonOpOptA name o = mkMonOpOptA name (Identity . o)

pureDyadOpOptA :: String -> ((Either Array Function) -> (Either Array Function) -> Function) -> Operator
pureDyadOpOptA name o = mkDyadOpOptA name (Identity .: o)

{- Functions -}

-- specialized
fImplicitGroup = pureMonFn (mkFnInfoM ")(") F.implicitGroup
fGetString = F.getString

-- double-as-operators
{-
fReplicate = pureDyadFn "/" (dFPH "/")
fExpand = pureDyadFn "\\" (dFPH "\\")
fReplicateFirst = pureDyadFn "⌿" (dFPH "⌿")
fExpandFirst = pureDyadFn "⍀" (dFPH "⍀")
-}

-- inverses
iAsteriskM = Just $ pureFuncM F.naturalLog
iAsteriskD = Just $ pureFuncD F.logBase

-- on-axis
reverseOnAxis = Just (\ax -> mkFuncM $ F.reverse ax)
rotateOnAxis = Just (\ax -> mkFuncD $ F.rotate ax)

-- primitive
fPlus = pureMonDyadFn (mkFnInfoA "+") {fnIdAD = Just $ ScalarNum 0} F.conjugate F.add
fMinus = pureMonDyadFn (mkFnInfoA "-") {fnIdAD = Just $ ScalarNum 0} F.negate F.subtract
fTimes = pureMonDyadFn (mkFnInfoA "×") {fnIdAD = Just $ ScalarNum 1} F.direction F.multiply
fDivide = pureMonDyadFn (mkFnInfoA "÷") {fnIdAD = Just $ ScalarNum 1} F.reciprocal F.divide
fIota = mkMonDyadFn (mkFnInfoA "⍳") F.iota F.indexOf
fRho = pureMonDyadFn (mkFnInfoA "⍴") F.shapeOf F.reshape
fLss = pureDyadFn (mkFnInfoD "<") {fnIdD = Just $ ScalarNum 0} F.lss
fLeq = pureDyadFn (mkFnInfoD "≤") {fnIdD = Just $ ScalarNum 1} F.leq
fGtr = pureDyadFn (mkFnInfoD ">") {fnIdD = Just $ ScalarNum 0} F.gtr
fGeq = pureDyadFn (mkFnInfoD "≥") {fnIdD = Just $ ScalarNum 1} F.geq
fEqu = pureDyadFn (mkFnInfoD "=") {fnIdD = Just $ ScalarNum 1} F.equ
fAsterisk = pureMonDyadFn (mkFnInfoA "*") {fnIdAD = Just $ ScalarNum 1, fnInverseAM = iAsteriskM, fnInverseAD = iAsteriskD} F.exponential F.power
fAsteriskCircle = pureMonDyadFn (mkFnInfoA "⍟") F.naturalLog F.logBase
fFloor = pureMonDyadFn (mkFnInfoA "⌊") {fnIdAD = Just $ ScalarNum floatMax} F.floor F.minimum
fCeil = pureMonDyadFn (mkFnInfoA "⌈") {fnIdAD = Just $ ScalarNum floatMin} F.ceiling F.maximum
fRightTack = pureMonDyadFn (mkFnInfoA "⊢") F.identity F.right
fLeftTack = pureMonDyadFn (mkFnInfoA "⊣") F.identity F.left
fPipe = pureMonDyadFn (mkFnInfoA "|") {fnIdAD = Just $ ScalarNum 0} F.absoluteValue F.residue
fTripleEqu = pureMonDyadFn (mkFnInfoA "≡") F.depth F.match
fTripleNeq = pureMonDyadFn (mkFnInfoA "≢") F.tally F.notMatch
fNand = pureDyadFn (mkFnInfoD "⍲") F.nand
fNor = pureDyadFn (mkFnInfoD "⍱") F.nor
fAnd = pureDyadFn (mkFnInfoD "∧") {fnIdD = Just $ ScalarNum 1} F.lcm
fOr = pureDyadFn (mkFnInfoD "∨") {fnIdD = Just $ ScalarNum 0} F.gcd
fCircle = pureMonDyadFn (mkFnInfoA "○") F.piTimes F.circularFormulae
fBang = pureMonDyadFn (mkFnInfoA "!") {fnIdAD = Just $ ScalarNum 1} F.factorial F.binomial
fQuestion = mkMonDyadFn (mkFnInfoA "?") F.roll F.deal
fEncode = pureDyadFn (mkFnInfoD "⊤") F.encode
fTranspose = mkMonDyadFn (mkFnInfoA "⍉") {fnCanSelectAM = True, fnCanSelectAD = True} F.transpose F.reorderAxes
fReverse = mkMonDyadFn (mkFnInfoA "⌽") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAM = reverseOnAxis, fnOnAxisAD = rotateOnAxis} F.reverseLast F.rotateLast
fReverseFirst = mkMonDyadFn (mkFnInfoA "⊖") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAM = reverseOnAxis, fnOnAxisAD = rotateOnAxis} F.reverseFirst F.rotateFirst
fEpsilon = pureMonDyadFn (mkFnInfoA "∊") F.enlist F.membership
fGradeUp = mkMonDyadFn (mkFnInfoA "⍋") F.gradeUpM F.gradeUpD
fGradeDown = mkMonDyadFn (mkFnInfoA "⍒") F.gradeDownM F.gradeDownD
fCup = pureMonDyadFn (mkFnInfoA "∪") F.unique F.union
fCap = pureDyadFn (mkFnInfoD "∩") F.intersection
fTilde = pureMonDyadFn (mkFnInfoA "~") F.logicalNegate F.without
fNeq = pureMonDyadFn (mkFnInfoA "≠") F.uniqueMask F.neq
fExecute = MonFn (mkFnInfoM "⍎") F.execute
fFormat = pureMonFn (mkFnInfoM "⍕") F.format

functionGlyphs :: [(Char, Function)]
functionGlyphs = [
        ('+', fPlus),
        ('-', fMinus),
        ('×', fTimes),
        ('÷', fDivide),
        ('⍳', fIota),
        ('⍴', fRho),
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
        ('?', fQuestion),
        ('⊤', fEncode),
        ('⍉', fTranspose),
        ('⌽', fReverse),
        ('⊖', fReverseFirst),
        ('∊', fEpsilon),
        ('⍋', fGradeUp),
        ('⍒', fGradeDown),
        ('∪', fCup),
        ('∩', fCap),
        ('~', fTilde),
        ('≠', fNeq),
        ('⍎', fExecute),
        ('⍕', fFormat)
        -- TODO big list of functions
    ]

{- Operators -}

{-
oReduce = pureMonOp "/" (mOPH "/")
oScan = pureMonOp "\\" (mOPH "\\")
oReduceFirst = pureMonOp "⌿" (mOPH "⌿")
oScanFirst = pureMonOp "⍀" (mOPH "⍀")
-}

oSelfie = pureMonOpOptA "⍨" O.selfie
oAtop = pureDyadOpOptA "⍤" O.atop
oOver = pureDyadOp "⍥" O.over
oJot = pureDyadOpOptA "∘" O.jot
oEach = pureMonOp "¨" O.each
oPower = pureDyadOpOptA "⍣" O.power

-- oAxisSpec axis = pureMonOp ("[" ++ (show axis) ++ "]") (mOPH "[]") -- TODO remove `axis' arg and add arg to called fn

operatorGlyphs :: [(Char, Operator)]
operatorGlyphs = [
        ('⍨', oSelfie),
        ('⍤', oAtop),
        ('⍥', oOver),
        ('∘', oJot),
        ('¨', oEach),
        ('⍣', oPower)
        -- TODO big list of operators
    ]

opOrFnGlyphs :: [(Char, Operator, Function)]
opOrFnGlyphs = [
{-
        ('/', oReduce, fReplicate),
        ('⌿', oReduceFirst, fReplicateFirst),
        ('\\', oScan, fExpand),
        ('⍀', oScanFirst, fExpandFirst)
-}
    ]
