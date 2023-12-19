module Parse where
import Lex (Token(..))
import GrammarTree
import Glyphs
import Eval
import Data.List (isPrefixOf)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad

{-
 - statement => {expr}
 -
 - expr => [der_arr | train | op] [⍝ <ignore-until-eoe>] (eoe)
 -
 - dfn_expr => [der_arr | guard | alpha_ass | fn_ass | op_ass ] [⍝ <ignore-until-eoe>] (eoe)
 -
 - dfn_decl => { <skip tokens> }             (result is operator iff ⍺⍺ or ⍵⍵ ∊ tokens)
 -
 - guard => der_arr : der_arr
 -
 - index_list => (der_arr|;)*
 -
 - der_arr => arr_ass
 -         => ⎕ ← der_arr
 -         => der_fn der_arr
 -         => arr der_fn der_arr
 -         => arr
 -
 - arr_ass => ID ← der_arr
 -         => ID der_fn ← der_arr
 -
 - train => [df] {(arr|df) df} df            (where df = der_fn)
 -
 - der_fn => (f|a) op [f|a] {op [f|a]}       (where (f|a) is fn or arr; match the
 -                                            optional iff op is dyadic)
 -                                           (∘ may match (f|a) if the following op is .
 -                                            (this allows for outer product))
 -                                           (∘ must not match op if . follows it)
 -        => fn
 -
 - op => ¨ ⍨ ⌸ ⌶                             (monadic)
 -    => ⍣ . ∘ ⍤ ⍥ @ ⍠ ⌺                     (dyadic)
 -    => [der_arr]                           (monadic)
 -    => dfn_decl                            (if dfn_decl is op)
 -    => ∇∇                                  (if ∇∇ is in id map)
 -    => op_ass
 -    => ⎕ ← op
 -    => ID                                  (if ID is op)
 -    => op_or_fn
 -    => (op)
 -
 - op_ass => ID ← op
 -
 - fn => = ≤ < > ≥ ∨ ∧ ⍲ ⍱ ⍷ ∩ ←             (dyadic)
 -    => + - × ÷ * ⍟ ⌹ ○ ! ? | ⌈ ⌊ ⊥ ⊤       (monadic or dyadic)
 -    => ⊣ ⊢ ≠ ≡ ≢ ↑ ↓ ⊂ ⊃ ⊆ ⌷ ⍋ ⍒ ⍳ ⍸       (monadic or dyadic)
 -    => ∊ ∪ ~ , ⍪ ⍴ ⌽ ⊖ ⍉ ⍎ ⍕               (monadic or dyadic)
 -    => ⎕ID                                 (if ⎕ID is a d_fn)
 -    => ⍺⍺ | ⍵⍵ | ∇                         (if dfn_decl state matches these)
 -    => dfn_decl                            (if dfn_decl is fn)
 -    => fn_ass
 -    => ⎕ ← train
 -    => ID                                  (if ID is fn)
 -    => op_or_fn
 -    => (train)
 -
 - fn_ass => ID ← train
 -
 - op_or_fn => / ⌿ \ ⍀                       (monadic operators / dyadic functions)
 -
 - arr => arr_comp {arr_comp | [index_list]}
 -
 - arr_comp => scalar {scalar}
 -
 - scalar => NUM
 -        => STR
 -        => ID                            (if ID is arr)
 -        => ⎕ID                           (if ⎕ID is arr)
 -        => (der_arr)
 -        => ⍺ | ⍵                         (if ⍺/w is in namespace)
 -        => ⍺⍺ | ⍵⍵                       (if ⍺⍺/ww is in namespace and is array)
 -        => ⍬
 -
 - Tokens:
 - NUM: ¯?[0-9]*\.?[0-9]+
 - STR: '([^']|'')*'
 - ID: [a-zA-Z_][a-zA-Z_0-9]*
 - (all other non-whitespace tokens are read literally)
 -
 -}

{- Monad Helpers -}

evalMatchFn :: IdMap -> [Token] -> MatchFn a -> Maybe (a, [Token])
evalMatchFn idm toks = (flip runReader) idm . runMaybeT . (flip runStateT) toks

getIdm :: MatchFn IdMap
getIdm = lift . lift $ ask

{- Matching Functions -}

type MatchFn a = StateT [Token] (MaybeT (Reader IdMap)) a

maybeMatch :: MatchFn a -> MatchFn (Maybe a)
maybeMatch f = do
    idm <- getIdm
    toks <- get
    case evalMatchFn idm toks f of
        Nothing -> return Nothing
        Just (x, toks') -> do
            put toks'
            return $ Just x

matchOne :: [MatchFn a] -> MatchFn a
-- return first successful match, else Nothing
matchOne [] = mzero
matchOne (f:fs) = do
    mb <- maybeMatch f
    case mb of
        Nothing -> matchOne fs
        Just x -> return x

matchAll :: [MatchFn a] -> MatchFn [a]
matchAll [] = return []
matchAll (f:fs) = do
    a <- f
    as <- matchAll fs
    return $ a:as

matchMax :: [MatchFn a] -> MatchFn [[a]]
-- match 0 or more repetitions of the entire function list
matchMax fns = do
    mb <- maybeMatch $ matchAll fns
    case mb of
        Nothing -> return []
        Just x -> do
            xs <- matchMax fns
            return $ x:xs

matchAllThenMax :: [MatchFn a] -> MatchFn [[a]]
-- match 1 or more repetitions of entire function list
matchAllThenMax fns = do
    x <- matchAll fns
    xs <- matchMax fns
    return $ x:xs

{- Matching Tuples (hard-coded) -}

matchT2 :: (MatchFn a, MatchFn b) -> MatchFn (a, b)
matchT2 (fa, fb) = do
    a <- fa
    b <- fb
    return (a, b)

matchT3 :: (MatchFn a, MatchFn b, MatchFn c) -> MatchFn (a, b, c)
matchT3 (fa, fb, fc) = do
    a <- fa
    b <- fb
    c <- fc
    return (a, b, c)

matchT4 :: (MatchFn a, MatchFn b, MatchFn c, MatchFn d) -> MatchFn (a, b, c, d)
matchT4 (fa, fb, fc, fd) = do
    a <- fa
    b <- fb
    c <- fc
    d <- fd
    return (a, b, c, d)

{- Matching Specific Types -}

matchA :: (Token -> Maybe a) -> MatchFn a
matchA tokToMbA = do
    toks <- get
    case toks of
        (t:toks')
            | (Just x) <- tokToMbA t -> do
                put toks'
                return x
            | otherwise -> mzero
        [] -> mzero

matchCh :: Char -> MatchFn Char
matchCh c = matchA $ \t -> case t of
    ChTok ch | ch == c -> Just c
    _ -> Nothing

matchStrLiteral :: MatchFn String
matchStrLiteral = matchA $ \t -> case t of
    StrTok s -> Just s
    _ -> Nothing

matchId :: MatchFn String
matchId = matchA $ \t -> case t of
    IdTok s -> Just s
    _ -> Nothing

matchNumLiteral :: MatchFn Double
matchNumLiteral = matchA $ \t -> case t of
    NumTok n -> Just n
    _ -> Nothing

matchEoe :: MatchFn ()
matchEoe = do
    toks <- get
    case toks of
        [] -> return ()
        (ChTok '⋄':toks') -> put toks'
        (ChTok '\n':toks') -> put toks'
        _ -> mzero
    return ()

matchComment :: MatchFn ()
matchComment = do
    matchCh '⍝'
    matchMax [matchAnyTokenExcept [ChTok '\n']]
    matchEoe

matchCommentOrEoe :: MatchFn ()
matchCommentOrEoe = matchOne [matchComment, matchEoe]

mapGetIf :: String -> (IdEntry -> Maybe a) -> MatchFn a
mapGetIf s f = do
    idm <- getIdm
    case mapLookup s idm of
        Nothing -> mzero
        Just entry -> case f entry of
            Nothing -> mzero
            Just x -> return x

matchIdWith :: (IdEntry -> Maybe a) -> MatchFn a
matchIdWith f = do
    s <- matchId
    mapGetIf s f

matchQuadIdWith :: (IdEntry -> Maybe a) -> MatchFn a
matchQuadIdWith f = do
    matchCh '⎕'
    s <- matchId
    mapGetIf ('⎕':s) f

matchSpecialIdWith :: Token -> String -> (IdEntry -> Maybe a) -> MatchFn a
matchSpecialIdWith specTok s f = do
    matchA $ \t -> if t == specTok then Just () else Nothing
    mapGetIf s f

matchAnyTokenExcept :: [Token] -> MatchFn Token
matchAnyTokenExcept blacklist = matchA $ \t -> if t `elem` blacklist then Nothing else Just t

{- Data Types -}

data ExprResult = ResAtn ArrTreeNode Bool -- Bool = should print?
                | ResFtn FnTreeNode Bool
                | ResOp OpTreeNode Bool
                | ResNull

instance Show ExprResult where
    show (ResAtn a _) = show a
    show (ResFtn f _) = show f
    show (ResOp o _) = show o

data DfnExprResult = DResAtn ArrTreeNode Bool -- Bool = should return?
                   | DResCond ArrTreeNode ArrTreeNode
                   | DResDefaultAlpha ArrTreeNode
                   | DResFtn FnTreeNode -- no bool: can't return function (should be an assignment)
                   | DResOtn OpTreeNode -- nor operator                   (should be an assignment)
                   | DResNull

{- Parsing Functions -}

parseExpr :: MatchFn ExprResult
parseExpr = matchOne [
        (mkResAtn . fst) <$> matchT2 (parseDerArr, matchCommentOrEoe),
        (mkResFtn . fst) <$> matchT2 (parseTrain, matchCommentOrEoe),
        (mkResOp . fst) <$> matchT2 (parseOp, matchCommentOrEoe),
        (\_ -> ResNull) <$> matchCommentOrEoe
    ]
    where mkResAtn a@(ArrInternalAssignment _ _) = ResAtn a False
          mkResAtn a@(ArrInternalModAssignment _ _ _) = ResAtn a False
          mkResAtn a = ResAtn a True
          mkResFtn f@(FnInternalAssignment _ _) = ResFtn f False
          mkResFtn f = ResFtn f True
          mkResOp o@(OpInternalAssignment _ _) = ResOp o False
          mkResOp o = ResOp o True

parseDfnExpr :: MatchFn DfnExprResult
parseDfnExpr = matchOne [
        (mkDResAtn . fst) <$> matchT2 (parseDerArr, matchCommentOrEoe),
        ((\(a1, a2) -> DResCond a1 a2) . fst) <$> matchT2 (parseGuard, matchCommentOrEoe),
        (DResDefaultAlpha . fst) <$> matchT2 (parseAlphaAss, matchCommentOrEoe),
        (DResFtn . fst) <$> matchT2 (parseFnAss, matchCommentOrEoe),
        (DResOtn . fst) <$> matchT2 (parseOpAss, matchCommentOrEoe),
        (\_ -> DResNull) <$> matchCommentOrEoe
    ]
    where mkDResAtn a@(ArrInternalAssignment _ _) = DResAtn a False
          mkDResAtn a@(ArrInternalModAssignment _ _ _) = DResAtn a False
          mkDResAtn a = DResAtn a True

parseGuard :: MatchFn (ArrTreeNode, ArrTreeNode)
parseGuard = (\(a1, _, a2) -> (a1, a2)) <$> matchT3 (
        parseDerArr,
        matchCh ':',
        parseDerArr
    )

parseAlphaAss :: MatchFn ArrTreeNode
parseAlphaAss = (\(_, _, da) -> da) <$> matchT3 (
        matchCh '⍺',
        matchCh '←',
        parseDerArr
    )

parseDfnDecl :: MatchFn ([Token], Bool, Bool) -- toks, is_op, is_dyadic_op
parseDfnDecl = do
    matchCh '{'
    toks <- (concat . concat) <$> matchMax [
            matchOne [
                (:[]) <$> matchAnyTokenExcept [ChTok '{', ChTok '}'],
                (\(a, b, c) -> [ChTok '{'] ++ a ++ [ChTok '}']) <$> parseDfnDecl
            ]
        ]
    matchCh '}'
    return (toks, AATok `elem` toks, WWTok `elem` toks)

parseIdxList :: MatchFn [ArrTreeNode]
parseIdxList = (foldIdxList . concat) <$> matchMax [
        matchOne [
            (Right) <$> matchCh ';',
            (Left) <$> parseDerArr
        ]
    ]
    where emptyArr = ArrLeaf . arrFromList $ []
          foldIdxList [] = [emptyArr]
          foldIdxList (Left arr:[]) = [arr]
          foldIdxList (Right _:rest) = emptyArr : foldIdxList rest
          foldIdxList (Left arr:Right _:rest) = arr : foldIdxList rest

parseDerArr :: MatchFn ArrTreeNode
parseDerArr = matchOne [
        parseArrAss,
        (\(f, da) -> ArrInternalMonFn f da) <$> matchT2 (parseDerFn, parseDerArr),
        (\(lhs, f, rhs) -> ArrInternalDyadFn f lhs rhs) <$> matchT3 (
            parseArr,
            parseDerFn,
            parseDerArr
        ),
        parseArr
    ]

parseArrAss :: MatchFn ArrTreeNode
parseArrAss = matchOne [
        (\(id, _, da) -> ArrInternalAssignment id da) <$> matchT3 (
            matchId,
            matchCh '←',
            parseDerArr
        ),
        (\(id, df, _, da) -> ArrInternalModAssignment id df da) <$> matchT4 (
            matchId,
            parseDerFn,
            matchCh '←',
            parseDerArr
        )
    ]

parseTrain :: MatchFn FnTreeNode
parseTrain = (tranify) <$> matchOne [
        (\(f, t) -> f : t) <$> matchT2 (
            parseDerFn,
            _matchTail
        ),
        _matchTail
    ] where
    _matchTail = do -- {(arr|df) df} df
        fs <- (concat) <$> matchMax [
               matchOne [
                   (FnLeafArr) <$> parseArr,
                   parseDerFn
               ],
               parseDerFn
           ]
        f <- parseDerFn
        return $ fs ++ [f]
    tranify nodes
        | length nodes == 1 = head nodes
        | length nodes == 2 = FnInternalAtop (nodes !! 0) (nodes !! 1)
        | even . length $ nodes = FnInternalAtop (head nodes) (forkify . tail $ nodes)
        | otherwise = forkify nodes
        where forkify (n1:n2:n3:[]) = FnInternalFork n1 n2 n3
              forkify (n1:n2:rest) = FnInternalFork n1 n2 (forkify rest)

parseDerFn :: MatchFn FnTreeNode
parseDerFn = matchOne [_parseOpExpr, parseFn]
    where _parseOpExpr = do
              lhs <- _parseArg
              otn <- parseOp
              _parseOpExprRec lhs otn
          _parseArg = matchOne [parseFn, FnLeafArr <$> parseArr]
          _parseOpExprRec :: FnTreeNode -> OpTreeNode -> MatchFn FnTreeNode
          _parseOpExprRec lhs otn = do
              df <- case unwrapOpTree otn of
                        (MonOp _ _) -> return $ FnInternalMonOp otn lhs
                        (DyadOp _ _) -> do rhs <- _parseArg
                                           return $ FnInternalDyadOp otn lhs rhs
              mb <- maybeMatch parseOp
              case mb of
                   Nothing -> return $ df
                   (Just otn2) -> _parseOpExprRec df otn2

parseOp :: MatchFn OpTreeNode
parseOp = matchOne [
        matchOne $ map (\(c, o) -> (\_ -> OpLeaf o) <$> matchCh c) operatorGlyphs,
        (\(_, da, _) -> OpLeaf $ oAxisSpec da) <$> matchT3 (
            matchCh '[',
            parseDerArr,
            matchCh ']'
        ),
        -- dfn_decl
        dfnDeclToOp =<< parseDfnDecl,
        -- ∇∇
        matchSpecialIdWith DDTok "∇∇" idEntryToOtn,
        parseOpAss,
        matchIdWith (idEntryToOtn),
        (OpLeaf . fst) <$> parseOpOrFn,
        (\(_, o, _) -> OpInternalDummyNode o) <$> matchT3 (
            matchCh '(',
            parseOp,
            matchCh ')'
        )
    ]
    where idEntryToOtn (IdOp o) = Just (OpLeaf o)
          idEntryToOtn (IdTokList idtfs toks True is_dy) = Just . OpLeaf $ mkDop idtfs toks is_dy
          idEntryToOtn _ = Nothing
          dfnDeclToOp (toks, True, is_dy) = return . OpLeaf $ mkDop [] toks is_dy
          dfnDeclToOp _ = mzero

parseOpAss :: MatchFn OpTreeNode
parseOpAss = (\(id, _, op) -> OpInternalAssignment id op) <$> matchT3 (
        matchId,
        matchCh '←',
        parseOp
    )

parseFn :: MatchFn FnTreeNode
parseFn = matchOne [
        matchOne $ map (\(c, f) -> (\_ -> FnLeafFn f) <$> matchCh c) functionGlyphs,
        matchQuadIdWith (idEntryToFnTree),
        -- ⍺⍺ | ⍵⍵
        matchSpecialIdWith (AATok) "⍺⍺" (idEntryToFnTree),
        matchSpecialIdWith (WWTok) "⍵⍵" (idEntryToFnTree),
        -- ∇
        matchSpecialIdWith (ChTok '∇') "∇" idEntryToFnTree,
        -- dfn_decl
        dfnDeclToFn =<< parseDfnDecl,
        -- ID ← train
        parseFnAss,
        -- ID
        matchIdWith (idEntryToFnTree),
        -- op_or_fn
        (FnLeafFn . snd) <$> parseOpOrFn,
        -- (train)
        (\(_, t, _) -> FnInternalDummyNode t) <$> matchT3 (
            matchCh '(',
            parseTrain,
            matchCh ')'
        )
    ]
    where idEntryToFnTree (IdFn f) = Just $ FnLeafFn f
          idEntryToFnTree (IdTokList idtfs toks False False) = Just . FnLeafFn . mkDfn idtfs $ toks
          idEntryToFnTree _ = Nothing
          dfnDeclToFn (toks, False, False) = return . FnLeafFn . mkDfn [] $ toks
          dfnDeclToFn _ = mzero

parseFnAss :: MatchFn FnTreeNode
parseFnAss = (\(id, _, ftn) -> FnInternalAssignment id ftn) <$> matchT3 (
        matchId,
        matchCh '←',
        parseTrain
    )

parseOpOrFn :: MatchFn (Operator, Function)
parseOpOrFn = matchOne $ map (\(c, o, f) -> (\_ -> (o, f)) <$> matchCh c) opOrFnGlyphs

parseArr :: MatchFn ArrTreeNode -- parse an entire literal array
parseArr = (_roll) <$> matchT2 (
        parseArrComp,
        (concat) <$> matchMax [ matchOne [
            (Left) <$> parseArrComp,
            (\(_, il, _) -> Right il) <$> matchT3 (
                matchCh '[',
                parseIdxList,
                matchCh ']'
            )
        ]]
    )
    where _roll (a, xs) = foldl (_merge) a xs
          _merge a (Right i) = ArrInternalSubscript a i
          _merge a (Left a2) = ArrInternalImplCat a a2

parseArrComp :: MatchFn ArrTreeNode -- parse array "component"
parseArrComp = (applyImplCat . concat) <$> matchAllThenMax [parseScalar]
    where applyImplCat as@(_:_) = foldr (ArrInternalImplCat) (last as) (init as)

parseScalar :: MatchFn ArrTreeNode
parseScalar = matchOne [
            -- NUM
            (\n -> ArrLeaf . arrFromList . (:[]) . ScalarNum $ n) <$> matchNumLiteral,
            -- STR
            (toScalarStr . map ScalarCh) <$> matchStrLiteral,
            -- ID
            matchIdWith (idEntryToArrTree),
            -- ⎕ID
            matchQuadIdWith (idEntryToArrTree),
            -- ⍺ | ⍵
            matchSpecialIdWith (ChTok '⍺') "⍺" (idEntryToArrTree),
            matchSpecialIdWith (ChTok '⍵') "⍵" (idEntryToArrTree),
            -- ⍺⍺ | ⍵⍵
            matchSpecialIdWith AATok "⍺⍺" (idEntryToArrTree),
            matchSpecialIdWith WWTok "⍵⍵" (idEntryToArrTree),
            -- ⍬
            (\_ -> ArrInternalMonFn (FnLeafFn fImplicitGroup) $ (ArrLeaf . arrFromList) []) <$> matchCh '⍬',
            -- (der_arr)
            (\(_, da, _) -> ArrInternalMonFn (FnLeafFn fImplicitGroup) da) <$> matchT3 (
                matchCh '(',
                parseDerArr,
                matchCh ')'
            )
    ]
    where toScalarStr (c:[]) = ArrLeaf . arrFromList $ [c]
          toScalarStr s = ArrInternalMonFn (FnLeafFn fImplicitGroup) (ArrLeaf . arrFromList $ s)
          idEntryToArrTree (IdArr a) = Just $ ArrLeaf a
          idEntryToArrTree _ = Nothing
