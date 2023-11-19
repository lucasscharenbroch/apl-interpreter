module Parse where
import Lex (Token(..))
import GrammarTree
import Glyphs

{-
 -
 - expr => [([⎕ ←] der_arr) | train | op] [⍝ <ignore-until-EOS> | EOS ]
 -
 - dfn_decl => { <skip tokens> }             (result is operator iff ⍺⍺ or ⍵⍵ ∊ tokens)
 -
 - index_list => (der_arr|;)*
 -
 - der_arr => der_fn der_arr
 -         => arr der_fn der_arr
 -         => ID ← der_arr
 -         => arr
 -
 - train => {df df} df                       (nested forks) (where df = der_fn)
 -       => df {df df} df                    (atop, nested forks)
 -       => arr df {df df} df
 -       => df
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
 -    => (ID ← dfn_decl)                     (if dfn_decl is op)
 -    => op_or_fn
 -
 - fn => = ≤ < > ≥ ∨ ∧ ⍲ ⍱ ⍷ ∩ ←             (dyadic)
 -    => + - × ÷ * ⍟ ⌹ ○ ! ? | ⌈ ⌊ ⊥ ⊤       (monadic or dyadic)
 -    => ⊣ ⊢ ≠ ≡ ≢ ↑ ↓ ⊂ ⊃ ⊆ ⌷ ⍋ ⍒ ⍳ ⍸       (monadic or dyadic)
 -    => ∊ ∪ ~ , ⍪ ⍴ ⌽ ⊖ ⍉ ⍎ ⍕               (monadic or dyadic)
 -    => ⎕ID                                 (if ⎕ID is a d_fn)
 -    => ⍺⍺ | ⍵⍵ | ∇                         (if dfn_decl state matches these)
 -    => dfn_decl                            (if dfn_decl is fn)
 -    => (ID ← dfn_decl)                     (if dfn_decl is fn)
 -    => (train)
 -    => op_or_fn
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

{- Matching Functions -}

chFst :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
-- chain first: apply function to first element of maybe-wrapped tuple
chFst f m = case m of
    Nothing -> Nothing
    Just (x, y) -> Just (f x, y)

mchFst :: (a -> Maybe b) -> Maybe (a, c) -> Maybe (b, c)
-- maybe chain first
mchFst f m = case m of
    Nothing -> Nothing
    Just (x, y) -> case (f x) of
        Nothing -> Nothing
        Just z -> Just(z, y)

type MatchFn a = (IdMap, [Token]) -> Maybe (a, [Token])

matchOne :: [MatchFn a] -> MatchFn a
-- return first successful match, else Nothing
matchOne fns args = foldl try Nothing fns
    where try (Just x) _ = Just x -- already found match
          try Nothing f = f args

matchAll :: [MatchFn a] -> MatchFn [a]
-- match every function in list (sequentially), returning their results, else Nothing
matchAll fns (idm, toks) = chFst (reverse) . foldl try (Just ([], toks)) $ fns
    where try Nothing _ = Nothing
          try (Just (rs, ts)) f = case f (idm, ts) of
              Just (r, ts') -> Just(r:rs, ts')
              _ -> Nothing

matchMax :: [MatchFn a] -> MatchFn [[a]]
-- match 0 or more repetitions of the entire function list
matchMax fns (idm, toks) = case matchAll fns (idm, toks) of
    Nothing -> Just ([], toks)
    Just (r, ts) -> case matchMax fns (idm, ts) of
        Nothing -> Just ([r], ts)
        Just (rs, ts') -> Just (r:rs, ts')

matchAllThenMax :: [MatchFn a] -> MatchFn [[a]]
-- match 1 or more repetitions of entire function list
matchAllThenMax fns = chFst (concat) . matchAll [
        chFst (:[]) . matchAll fns,
        matchMax fns
    ]

{- Tuple Matching (hard-coded) -}

matchT2 :: (MatchFn a, MatchFn b) -> MatchFn (a, b)
matchT2 (fa, fb) (idm, ts) = case fa (idm, ts) of
    Nothing -> Nothing
    Just (a, ts') -> case fb (idm, ts') of
        Nothing -> Nothing
        Just (b, ts'') -> Just ((a, b), ts'')

matchT3 :: (MatchFn a, MatchFn b, MatchFn c) -> MatchFn (a, b, c)
matchT3 (fa, fb, fc) (idm, ts) = case fa (idm, ts) of
    Nothing -> Nothing
    Just (a, ts') -> case fb (idm, ts') of
        Nothing -> Nothing
        Just (b, ts'') -> case fc (idm, ts'') of
            Nothing -> Nothing
            Just (c, ts''') -> Just ((a, b, c), ts''')

matchT4 :: (MatchFn a, MatchFn b, MatchFn c, MatchFn d) -> MatchFn (a, b, c, d)
matchT4 (fa, fb, fc, fd) (idm, ts) = case fa (idm, ts) of
    Nothing -> Nothing
    Just (a, ts') -> case fb (idm, ts') of
        Nothing -> Nothing
        Just (b, ts'') -> case fc (idm, ts'') of
            Nothing -> Nothing
            Just (c, ts''') -> case fd (idm, ts''') of
                Nothing -> Nothing
                Just (d, ts'''') -> Just ((a, b, c, d), ts'''')

{- Matching Specific Types -}

matchCh :: Char -> MatchFn Char
matchCh c (_, (ChTok c':ts))
    | c == c' = Just (c, ts)
    | otherwise = Nothing
matchCh _ _ = Nothing

matchStrLiteral :: MatchFn String
matchStrLiteral (_, (StrTok s:ts)) = Just (s, ts)
matchStrLiteral _ = Nothing

matchId :: MatchFn String
matchId (_, (IdTok idt:ts)) = Just (idt, ts)
matchId _ = Nothing

matchNumLiteral :: MatchFn (Either Int Double)
matchNumLiteral (_, (NumTok n:ts)) = Just (n, ts)
matchNumLiteral _ = Nothing

matchEos :: MatchFn ()
matchEos (_, []) = Just ((), [])
matchEos _ = Nothing

matchComment :: MatchFn ()
matchComment (_, (ChTok '⍝':_)) = Just((), [])
matchComment _ = Nothing

matchIdWith :: (IdEntry -> Maybe a) -> MatchFn a
matchIdWith f (idm, ((IdTok id):ts)) = case mapLookup id idm of
    Nothing -> Nothing
    Just (entry) -> case f entry of
        Nothing -> Nothing
        (Just x) -> Just (x, ts)
matchIdWith _ _ = Nothing

matchQuadIdWith :: (IdEntry -> Maybe a) -> MatchFn a
matchQuadIdWith f (idm, (ChTok '⎕':(IdTok id):ts)) = case mapLookup ('⎕' : id) idm of
    Nothing -> Nothing
    Just (entry) -> case f entry of
        Nothing -> Nothing
        (Just x) -> Just (x, ts)
matchQuadIdWith _ _ = Nothing

{- Data Types -}

data ExprResult = ResAtn ArrTreeNode
                | ResSilentAtn ArrTreeNode
                | ResFtn FnTreeNode
                | ResOp Operator
                | ResNull

instance Show ExprResult where
    show (ResAtn a) = show a
    show (ResSilentAtn a) = show a
    show (ResFtn f) = show f
    show (ResOp o) = show o

{- Parsing Functions -}

-- TODO here

parseExpr :: (IdMap, [Token]) -> Maybe ExprResult
parseExpr = (=<<) (Just . fst) . matchOne [
        chFst (\(_, _, a, _) -> ResAtn . ArrInternalMonFn (FnLeafFn fAssignToQuad) $ a) . matchT4 (
            matchCh '⎕',
            matchCh '←',
            parseDerArr,
            matchOne [matchComment, matchEos]
        ),
        chFst (\(atn, _) -> atnToRes atn) . matchT2 (parseDerArr, matchOne [matchComment, matchEos]),
        chFst (\(ftn, _) -> ResFtn ftn) . matchT2(parseTrain, matchOne [matchComment, matchEos]),
        chFst (\(op, _) -> ResOp op) . matchT2(parseOp, matchOne [matchComment, matchEos]),
        chFst (\_ -> ResNull) . matchOne [matchComment, matchEos]
    ]
    where atnToRes a@(ArrInternalAssignment _ _) = ResSilentAtn a
          atnToRes a = ResAtn a

-- parseDfnDecl :: [Token] -> Maybe (???, [Token])
-- parseDfnExpr :: [Token] -> Maybe (???, [Token])

parseIdxList :: MatchFn [ArrTreeNode]
parseIdxList = chFst (foldIdxList . concat) . matchMax [
        matchOne [
            chFst (Right) . matchCh ';',
            chFst (Left) . parseDerArr
        ]
    ]
    where emptyArr = ArrLeaf . arrFromList $ []
          foldIdxList [] = [emptyArr]
          foldIdxList (Left arr:[]) = [arr]
          foldIdxList (Right _:rest) = emptyArr : foldIdxList rest
          foldIdxList (Left arr:Right _:rest) = arr : foldIdxList rest

parseDerArr :: MatchFn ArrTreeNode
parseDerArr = matchOne [
        chFst (\(f, da) -> ArrInternalMonFn f da) . matchT2 (parseDerFn, parseDerArr),
        chFst (\(lhs, f, rhs) -> ArrInternalDyadFn f lhs rhs) . matchT3 (
            parseArr,
            parseDerFn,
            parseDerArr
        ),
        chFst (\(id, _, da) -> ArrInternalAssignment id da) . matchT3 (
            matchId,
            matchCh '←',
            parseDerArr
        ),
        parseArr
    ]

parseTrain :: MatchFn FnTreeNode
parseTrain = matchOne [
        chFst (tranify . (\(a, dfss) ->  FnLeafArr a : concat dfss)) . matchT2 ( -- arr df {df df} df
            parseArr,
            matchAllThenMax [parseDerFn, parseDerFn]
        ),
        chFst (tranify . concat) . matchAllThenMax [parseDerFn]
    ] where
    tranify nodes
        | length nodes == 1 = head nodes
        | length nodes == 2 = FnInternalAtop (nodes !! 0) (nodes !! 1)
        | even . length $ nodes = FnInternalAtop (head nodes) (forkify . tail $ nodes)
        | otherwise = forkify nodes
        where forkify (n1:n2:n3:[]) = FnInternalFork n1 n2 n3
              forkify (n1:n2:rest) = FnInternalFork n1 n2 (forkify rest)

parseDerFn :: MatchFn FnTreeNode
parseDerFn (idm, ts) = matchOne [
        (=<<) (parseDerFnRec) . (=<<) (finishOpMatch) . matchT2 (_parseArg, parseOp),
        parseFn
    ] (idm, ts)
    where _parseArg = matchOne [parseFn, chFst (FnLeafArr) . parseArr]
          finishOpMatch :: ((FnTreeNode, Operator), [Token]) -> Maybe (FnTreeNode, [Token])
          finishOpMatch ((lhs, op@(DyadOp _ _)), toks) = chFst (FnInternalDyadOp op lhs) $ _parseArg (idm, toks)
          finishOpMatch ((lhs, op), toks) = Just (FnInternalMonOp op lhs, toks)
          parseDerFnRec :: (FnTreeNode, [Token]) -> Maybe (FnTreeNode, [Token])
          parseDerFnRec (lhs, toks) = case (=<<) (finishOpMatch) . chFst (\op -> (lhs, op)) $ parseOp (idm, toks) of
              Nothing -> Just (lhs, toks)
              Just res -> parseDerFnRec res

parseOp :: MatchFn Operator
parseOp = matchOne [
        -- TODO big list of operators
        chFst (\_ -> oSelfie) . matchCh '⍨',
        chFst (\_ -> oAtop) . matchCh '⍤',
        chFst (\(_, da, _) -> oAxisSpec da) . matchT3 (
            matchCh '[',
            parseDerArr,
            matchCh ']'
        ),
        -- TODO dfn_decl
        -- TODO (ID ← dfn_decl)
        chFst (fst) . parseOpOrFn
    ]

parseFn :: MatchFn FnTreeNode
parseFn = matchOne [
        chFst (\_ -> FnLeafFn fPlus) . matchCh '+',
        chFst (\_ -> FnLeafFn fMinus) . matchCh '-',
        chFst (\_ -> FnLeafFn fTimes) . matchCh '×',
        chFst (\_ -> FnLeafFn fDivide) . matchCh '÷',
        chFst (\_ -> FnLeafFn fIota) . matchCh '⍳',
        chFst (\_ -> FnLeafFn fShape) . matchCh '⍴',
        -- TODO big list of functions
        matchIdWith (idEntryToFnTree),
        matchQuadIdWith (idEntryToFnTree),
        -- TODO ⍺⍺ ⌊ ⍵⍵ | ∇
        -- TODO (ID ← dfn_decl)
        chFst (\(_, t, _) -> t) . matchT3 (
            matchCh '(',
            parseTrain,
            matchCh ')'
        ),
        chFst (snd) . parseOpOrFn
    ]
    where idEntryToFnTree (IdFn f) = Just $ FnLeafFn f
          idEntryToFnTree _ = Nothing

parseOpOrFn :: MatchFn (Operator, FnTreeNode)
parseOpOrFn = matchOne [
        chFst (\_ -> (oReduce, FnLeafFn fReplicate)) . matchCh '/',
        chFst (\_ -> (oScan, FnLeafFn fExpand)) . matchCh '\\',
        chFst (\_ -> (oReduceFirst, FnLeafFn fReplicateFirst)) . matchCh '⌿',
        chFst (\_ -> (oScanFirst, FnLeafFn fExpandFirst)) . matchCh '⍀'
    ]

parseArr :: MatchFn ArrTreeNode -- parse an entire literal array
parseArr = chFst (_roll) . matchT2 (
        parseArrComp,
        chFst (concat) . matchMax [ matchOne [
            chFst (Left) . parseArrComp,
            chFst (\(_, il, _) -> Right il) . matchT3 (
                matchCh '[',
                parseIdxList,
                matchCh ']'
            )
        ]]
    )
        where _roll (a, xs) = foldl (_merge) a xs
              _merge a (Right i) = ArrInternalSubscript a i
              _merge a (Left a2) = ArrInternalDyadFn (FnLeafFn fImplicitCat) a a2

parseArrComp :: MatchFn ArrTreeNode -- parse array "component"
parseArrComp = chFst (applyImplCat . concat) . matchAllThenMax [parseScalar]
    where applyImplCat as@(_:_) = foldr (ArrInternalDyadFn (FnLeafFn fImplicitCat)) (last as) (init as)

parseScalar :: MatchFn ArrTreeNode
parseScalar = matchOne [
            -- NUM
            chFst (\n -> ArrLeaf . arrFromList . (:[]) . ScalarNum $ n) . matchNumLiteral,
            -- STR
            chFst (toScalarStr . map ScalarCh) . matchStrLiteral,
            -- ID
            matchIdWith (idEntryToArrTree),
            -- ⎕ID
            matchQuadIdWith (idEntryToArrTree),
            -- ⍺ | ⍵
            -- TODO (where ⍺ or ⍵ is in namespace)
            -- ⍺⍺ | ⍵⍵
            -- TODO (where ⍺⍺ or ⍵⍵ is in namespace and is array)
            -- ⍬
            chFst (\_ -> ArrInternalMonFn (FnLeafFn fImplicitGroup) $ (ArrLeaf . arrFromList) []) . matchCh '⍬',
            -- (der_arr)
            chFst (\(_, da, _) -> ArrInternalMonFn (FnLeafFn fImplicitGroup) da) . matchT3 (
                matchCh '(',
                parseDerArr,
                matchCh ')'
            )
    ]
    where toScalarStr (c:[]) = ArrLeaf . arrFromList $ [c]
          toScalarStr s = ArrInternalMonFn (FnLeafFn fImplicitGroup) (ArrLeaf . arrFromList $ s)
          idEntryToArrTree (IdArr a) = Just $ ArrLeaf a
          idEntryToArrTree _ = Nothing
