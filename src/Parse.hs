module Parse where
import Lex (Token(..))
import GrammarTree
import Glyphs
import Eval
import Data.List (isPrefixOf)

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

matchNumLiteral :: MatchFn Double
matchNumLiteral (_, (NumTok n:ts)) = Just (n, ts)
matchNumLiteral _ = Nothing

matchEoe :: MatchFn ()
matchEoe (_, []) = Just ((), [])
matchEoe (_, (ChTok '⋄':ts)) = Just ((), ts)
matchEoe (_, (ChTok '\n':ts)) = Just ((), ts)
matchEoe _ = Nothing

matchComment :: MatchFn ()
matchComment (idm, (ChTok '⍝':ts)) = chFst (\_ -> ()) . matchT2 (
        matchMax [matchAnyTokenExcept [ChTok '\n']],
        matchEoe
    ) $ (idm, ts)
matchComment _ = Nothing

matchCommentOrEoe :: MatchFn ()
matchCommentOrEoe = matchOne [ matchComment, matchEoe ]

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

matchSpecialIdWith :: [Token] -> String -> (IdEntry -> Maybe a) -> MatchFn a
matchSpecialIdWith specTs specId f (idm, ts) | specTs `isPrefixOf` ts = case mapLookup specId idm of
    Nothing -> Nothing
    Just (entry) -> case f entry of
        Nothing -> Nothing
        (Just x) -> Just (x, drop (length specTs) ts)
matchSpecialIdWith _ _ _ _ = Nothing

matchAnyTokenExcept :: [Token] -> MatchFn Token
matchAnyTokenExcept blacklist (_, (t:ts)) = if t `elem` blacklist
                                            then Nothing
                                            else Just (t, ts)
matchAnyTokenExcept _ (_, _) = Nothing

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
        chFst (mkResAtn . fst) . matchT2 (parseDerArr, matchCommentOrEoe),
        chFst (mkResFtn . fst) . matchT2 (parseTrain, matchCommentOrEoe),
        chFst (mkResOp . fst) . matchT2 (parseOp, matchCommentOrEoe),
        chFst (\_ -> ResNull) . matchCommentOrEoe
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
        chFst (mkDResAtn . fst) . matchT2 (parseDerArr, matchCommentOrEoe),
        chFst ((\(a1, a2) -> DResCond a1 a2) . fst) . matchT2 (parseGuard, matchCommentOrEoe),
        chFst (DResDefaultAlpha . fst) . matchT2(parseAlphaAss, matchCommentOrEoe),
        chFst (DResFtn . fst) . matchT2 (parseFnAss, matchCommentOrEoe),
        chFst (DResOtn . fst) . matchT2 (parseOpAss, matchCommentOrEoe),
        chFst (\_ -> DResNull) . matchCommentOrEoe
    ]
    where mkDResAtn a@(ArrInternalAssignment _ _) = DResAtn a False
          mkDResAtn a@(ArrInternalModAssignment _ _ _) = DResAtn a False
          mkDResAtn a = DResAtn a True

parseGuard :: MatchFn (ArrTreeNode, ArrTreeNode)
parseGuard = chFst (\(a1, _, a2) -> (a1, a2)) . matchT3 (
        parseDerArr,
        matchCh ':',
        parseDerArr
    )

parseAlphaAss :: MatchFn ArrTreeNode
parseAlphaAss = chFst (\(_, _, da) -> da) . matchT3 (
        matchCh '⍺',
        matchCh '←',
        parseDerArr
    )

parseDfnDecl :: MatchFn ([Token], Bool, Bool) -- toks, is_op, is_dyadic_op
parseDfnDecl = chFst (\(_, x, _) -> wrapToks x) . matchT3 (
        matchCh '{',
        chFst (concat . concat) . matchMax [
            matchOne [
                chFst (:[]) . matchAnyTokenExcept [ChTok '{', ChTok '}'],
                chFst (\(a, b, c) -> [ChTok '{'] ++ a ++ [ChTok '}']) . parseDfnDecl
            ]
        ],
        matchCh '}'
    )
    where wrapToks ts = (ts, aa || ww, ww)
            where aa = AATok `elem` ts
                  ww = WWTok `elem` ts

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
        parseArrAss,
        chFst (\(f, da) -> ArrInternalMonFn f da) . matchT2 (parseDerFn, parseDerArr),
        chFst (\(lhs, f, rhs) -> ArrInternalDyadFn f lhs rhs) . matchT3 (
            parseArr,
            parseDerFn,
            parseDerArr
        ),
        parseArr
    ]

parseArrAss :: MatchFn ArrTreeNode
parseArrAss = matchOne [
        chFst (\(id, _, da) -> ArrInternalAssignment id da) . matchT3 (
            matchId,
            matchCh '←',
            parseDerArr
        ),
        chFst (\(id, df, _, da) -> ArrInternalModAssignment id df da) . matchT4 (
            matchId,
            parseDerFn,
            matchCh '←',
            parseDerArr
        )
    ]

parseTrain :: MatchFn FnTreeNode
parseTrain = chFst (tranify) . matchOne [
        chFst (\(f, t) -> f : t) . matchT2 (
            parseDerFn,
            _matchTail
        ),
        _matchTail
    ] where
    _matchTail = chFst (concat) . matchAll [ -- {(arr|df) df} df
            chFst (concat) . matchMax [
                matchOne [
                    chFst (FnLeafArr) . parseArr,
                    parseDerFn
                ],
                parseDerFn
            ],
            chFst (:[]) . parseDerFn
        ]
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
          finishOpMatch :: ((FnTreeNode, OpTreeNode), [Token]) -> Maybe (FnTreeNode, [Token])
          finishOpMatch ((lhs, otn), toks) = case unwrapOpTree otn of
              (DyadOp _ _) -> chFst (FnInternalDyadOp otn lhs) $ _parseArg (idm, toks)
              (MonOp _ _) -> Just (FnInternalMonOp otn lhs, toks)
          parseDerFnRec :: (FnTreeNode, [Token]) -> Maybe (FnTreeNode, [Token])
          parseDerFnRec (lhs, toks) = case (=<<) (finishOpMatch) . chFst (\op -> (lhs, op)) $ parseOp (idm, toks) of
              Nothing -> Just (lhs, toks)
              Just res -> parseDerFnRec res

parseOp :: MatchFn OpTreeNode
parseOp = matchOne [
        matchOne $ map (\(c, o) -> chFst (\_ -> OpLeaf o) . matchCh c) operatorGlyphs,
        chFst (\(_, da, _) -> OpLeaf $ oAxisSpec da) . matchT3 (
            matchCh '[',
            parseDerArr,
            matchCh ']'
        ),
        -- dfn_decl
        mchFst (dfnDeclToOp) . parseDfnDecl,
        -- ∇∇
        matchSpecialIdWith [DDTok] "∇∇" idEntryToOtn,
        parseOpAss,
        matchIdWith (idEntryToOtn),
        chFst (OpLeaf . fst) . parseOpOrFn,
        chFst (\(_, o, _) -> OpInternalDummyNode o) . matchT3 (
            matchCh '(',
            parseOp,
            matchCh ')'
        )
    ]
    where idEntryToOtn (IdOp o) = Just (OpLeaf o)
          idEntryToOtn (IdTokList idtfs toks True is_dy) = Just . OpLeaf $ mkDop idtfs toks is_dy
          idEntryToOtn _ = Nothing
          dfnDeclToOp (toks, True, is_dy) = Just . OpLeaf $ mkDop [] toks is_dy
          dfnDeclToOp _ = Nothing

parseOpAss :: MatchFn OpTreeNode
parseOpAss = chFst (\(id, _, op) -> OpInternalAssignment id op) . matchT3 (
        matchId,
        matchCh '←',
        parseOp
    )

parseFn :: MatchFn FnTreeNode
parseFn = matchOne [
        matchOne $ map (\(c, f) -> chFst (\_ -> FnLeafFn f) . matchCh c) functionGlyphs,
        matchQuadIdWith (idEntryToFnTree),
        -- ⍺⍺ | ⍵⍵
        matchSpecialIdWith [AATok] "⍺⍺" (idEntryToFnTree),
        matchSpecialIdWith [WWTok] "⍵⍵" (idEntryToFnTree),
        -- ∇
        matchSpecialIdWith [ChTok '∇'] "∇" idEntryToFnTree,
        -- dfn_decl
        mchFst (dfnDeclToFn) . parseDfnDecl,
        -- ID ← train
        parseFnAss,
        -- ID
        matchIdWith (idEntryToFnTree),
        -- op_or_fn
        chFst (snd) . parseOpOrFn,
        -- (train)
        chFst (\(_, t, _) -> FnInternalDummyNode t) . matchT3 (
            matchCh '(',
            parseTrain,
            matchCh ')'
        )
    ]
    where idEntryToFnTree (IdFn f) = Just $ FnLeafFn f
          idEntryToFnTree (IdTokList idtfs toks False False) = Just . FnLeafFn . mkDfn idtfs $ toks
          idEntryToFnTree _ = Nothing
          dfnDeclToFn (toks, False, False) = Just . FnLeafFn . mkDfn [] $ toks
          dfnDeclToFn _ = Nothing

parseFnAss :: MatchFn FnTreeNode
parseFnAss = chFst (\(id, _, ftn) -> FnInternalAssignment id ftn) . matchT3 (
        matchId,
        matchCh '←',
        parseTrain
    )

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
            matchSpecialIdWith [ChTok '⍺'] "⍺" (idEntryToArrTree),
            matchSpecialIdWith [ChTok '⍵'] "⍵" (idEntryToArrTree),
            -- ⍺⍺ | ⍵⍵
            matchSpecialIdWith [AATok] "⍺⍺" (idEntryToArrTree),
            matchSpecialIdWith [WWTok] "⍵⍵" (idEntryToArrTree),
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
