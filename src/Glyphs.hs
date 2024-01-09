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

mkAmbivFn :: (SubEvalM m0, SubEvalM m1) => FnInfoA -> (Array -> m0 Array) -> (Array -> Array -> m1 Array) -> Function
mkAmbivFn ia fm fd = AmbivFn ia (\a -> toEvalM $ fm a) (\a b -> toEvalM $ fd a b)

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

pureAmbivFn :: FnInfoA -> (Array -> Array) -> (Array -> Array -> Array) -> Function
pureAmbivFn ia fm fd = mkAmbivFn ia (Identity . fm) (Identity .: fd)

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
iAsteriskCircleM = Just $ pureFuncM F.exponential
iAsteriskCircleD = Just $ pureFuncD F.power

-- on-axis
reverseOnAxis = Just (\ax -> mkFuncM $ F.reverse ax)
rotateOnAxis = Just (\ax -> mkFuncD $ F.rotate ax)
partitionedEncloseOnAxis = Just (\ax -> mkFuncD $ F.partitionedEnclose ax)
partitionOnAxis = Just (\ax -> mkFuncD $ F.partition ax)
catenateOnAxis = Just (\ax -> mkFuncD $ F.catenate ax)
replicateOnAxis = Just (\ax -> mkFuncD $ F.replicate ax)
expandOnAxis = Just (\ax -> mkFuncD $ F.expand ax)

-- primitive
fPlus = pureAmbivFn (mkFnInfoA "+") {fnIdAD = Just $ ScalarNum 0} F.conjugate F.add
fMinus = pureAmbivFn (mkFnInfoA "-") {fnIdAD = Just $ ScalarNum 0} F.negate F.subtract
fTimes = pureAmbivFn (mkFnInfoA "×") {fnIdAD = Just $ ScalarNum 1} F.direction F.multiply
fDivide = pureAmbivFn (mkFnInfoA "÷") {fnIdAD = Just $ ScalarNum 1} F.reciprocal F.divide
fIota = mkAmbivFn (mkFnInfoA "⍳") F.iota F.indexOf
fRho = pureAmbivFn (mkFnInfoA "⍴") F.shapeOf F.reshape
fLss = pureDyadFn (mkFnInfoD "<") {fnIdD = Just $ ScalarNum 0} F.lss
fLeq = pureDyadFn (mkFnInfoD "≤") {fnIdD = Just $ ScalarNum 1} F.leq
fGtr = pureDyadFn (mkFnInfoD ">") {fnIdD = Just $ ScalarNum 0} F.gtr
fGeq = pureDyadFn (mkFnInfoD "≥") {fnIdD = Just $ ScalarNum 1} F.geq
fEqu = pureDyadFn (mkFnInfoD "=") {fnIdD = Just $ ScalarNum 1} F.equ
fAsterisk = pureAmbivFn (mkFnInfoA "*") {fnIdAD = Just $ ScalarNum 1, fnInverseAM = iAsteriskM, fnInverseAD = iAsteriskD} F.exponential F.power
fAsteriskCircle = pureAmbivFn (mkFnInfoA "⍟") {fnInverseAM = iAsteriskCircleM, fnInverseAD = iAsteriskCircleD} F.naturalLog F.logBase
fFloor = pureAmbivFn (mkFnInfoA "⌊") {fnIdAD = Just $ ScalarNum floatMax} F.floor F.minimum
fCeil = pureAmbivFn (mkFnInfoA "⌈") {fnIdAD = Just $ ScalarNum floatMin} F.ceiling F.maximum
fRightTack = pureAmbivFn (mkFnInfoA "⊢") F.identity F.right
fLeftTack = pureAmbivFn (mkFnInfoA "⊣") F.identity F.left
fPipe = pureAmbivFn (mkFnInfoA "|") {fnIdAD = Just $ ScalarNum 0} F.absoluteValue F.residue
fTripleEqu = pureAmbivFn (mkFnInfoA "≡") F.depth F.match
fTripleNeq = pureAmbivFn (mkFnInfoA "≢") F.tally F.notMatch
fNand = pureDyadFn (mkFnInfoD "⍲") F.nand
fNor = pureDyadFn (mkFnInfoD "⍱") F.nor
fAnd = pureDyadFn (mkFnInfoD "∧") {fnIdD = Just $ ScalarNum 1} F.lcm
fOr = pureDyadFn (mkFnInfoD "∨") {fnIdD = Just $ ScalarNum 0} F.gcd
fCircle = pureAmbivFn (mkFnInfoA "○") F.piTimes F.circularFormulae
fBang = pureAmbivFn (mkFnInfoA "!") {fnIdAD = Just $ ScalarNum 1} F.factorial F.binomial
fQuestion = mkAmbivFn (mkFnInfoA "?") F.roll F.deal
fEncode = pureDyadFn (mkFnInfoD "⊤") F.encode
fDecode = pureDyadFn (mkFnInfoD "⊥") F.decode
fTranspose = mkAmbivFn (mkFnInfoA "⍉") {fnCanSelectAM = True, fnCanSelectAD = True} (Identity . F.transpose) F.reorderAxes
fReverse = mkAmbivFn (mkFnInfoA "⌽") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAM = reverseOnAxis, fnOnAxisAD = rotateOnAxis} F.reverseLast F.rotateLast
fReverseFirst = mkAmbivFn (mkFnInfoA "⊖") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAM = reverseOnAxis, fnOnAxisAD = rotateOnAxis} F.reverseFirst F.rotateFirst
fEpsilon = pureAmbivFn (mkFnInfoA "∊") F.enlist F.membership
fGradeUp = mkAmbivFn (mkFnInfoA "⍋") F.gradeUpM F.gradeUpD
fGradeDown = mkAmbivFn (mkFnInfoA "⍒") F.gradeDownM F.gradeDownD
fCup = pureAmbivFn (mkFnInfoA "∪") F.unique F.union
fCap = pureDyadFn (mkFnInfoD "∩") F.intersection
fTilde = pureAmbivFn (mkFnInfoA "~") F.logicalNegate F.without
fNeq = pureAmbivFn (mkFnInfoA "≠") F.uniqueMask F.neq
fExecute = MonFn (mkFnInfoM "⍎") F.execute
fFormat = pureMonFn (mkFnInfoM "⍕") F.format
fDisclose = mkAmbivFn (mkFnInfoA "⊃") {fnCanSelectAM = True, fnCanSelectAD = True} (Identity . F.first) F.pick
fEnclose = mkAmbivFn (mkFnInfoA "⊂") {fnOnAxisAD = partitionedEncloseOnAxis} (Identity . F.enclose) F.partitionedEncloseLast
fPartition = mkAmbivFn (mkFnInfoA "⊆") {fnOnAxisAD = partitionOnAxis} (Identity . F.nest) F.partitionLast
fComma = mkAmbivFn (mkFnInfoA ",") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAD = catenateOnAxis} (Identity . F.ravel) F.catenateLast
fCommaBar = mkAmbivFn (mkFnInfoA "⍪") {fnCanSelectAM = True, fnCanSelectAD = True, fnOnAxisAD = catenateOnAxis} (Identity . F.table) F.catenateFirst
fSquad = mkDyadFn (mkFnInfoD "⌷") {fnCanSelectD = True} F.index
fReplicate = mkDyadFn (mkFnInfoD "/") {fnOnAxisD = replicateOnAxis} F.replicateLast
fReplicateFirst = mkDyadFn (mkFnInfoD "⌿") {fnOnAxisD = replicateOnAxis} F.replicateFirst
fExpand = mkDyadFn (mkFnInfoD "\\") {fnOnAxisD = expandOnAxis} F.expandLast
fExpandFirst = mkDyadFn (mkFnInfoD "⍀") {fnOnAxisD = expandOnAxis} F.expandFirst
fIotaUnderbar = mkAmbivFn (mkFnInfoA "⍸") F.where_ F.intervalIndex

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
        ('⊥', fDecode),
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
        ('⍕', fFormat),
        ('⊃', fDisclose),
        ('⊂', fEnclose),
        ('⊆', fPartition),
        (',', fComma),
        ('⍪', fCommaBar),
        ('⌷', fSquad),
        ('⍸', fIotaUnderbar)
        -- TODO big list of functions
    ]

{- Operators -}

-- double-as-functions
oReduce = mkMonOp "/" O.reduceLast
oReduceFirst = mkMonOp "⌿" O.reduceFirst
oScan = mkMonOp "\\" O.scanLast
oScanFirst = mkMonOp "⍀" O.scanFirst

-- primitive
oSelfie = pureMonOpOptA "⍨" O.selfie
oAtop = pureDyadOpOptA "⍤" O.atop
oOver = pureDyadOp "⍥" O.over
oJot = pureDyadOpOptA "∘" O.jot
oEach = pureMonOp "¨" O.each
oPower = pureDyadOpOptA "⍣" O.power

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
        ('/', oReduce, fReplicate),
        ('⌿', oReduceFirst, fReplicateFirst),
        ('\\', oScan, fExpand),
        ('⍀', oScanFirst, fExpandFirst)
    ]
