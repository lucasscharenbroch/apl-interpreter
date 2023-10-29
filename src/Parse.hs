module Parse where
import Lex (Token(..))
import GrammarTree
import Glyphs

{-
 -
 - statement => expr <EOS>                   (EOS is end of statement: no tokens left)
 -
 - expr => ⎕ ← der_arr
 -      => der_arr | train | op
 -      => [expr] ⍝ <ignore-until-eol> (don't recursively match expr: check after above match is found)
 -      => expr {⋄ expr} [⋄]           (don't recursively match expr: check after above match is found)
 -
 - dfn_decl => { dfn_expr }
 -
 - dfn_expr => der_arr | train | op
 -          => der_arr : der_arr
 -          => [dfn_expr] ⍝ <discard-until-newline-or-⋄>
 -          => dfn_expr {⋄ dfn_expr} [⋄]
 -
 - index_list => (der_arr|;)*
 -
 - der_arr => train der_arr                  (train must be monadic)
 -         => arr der_fn der_arr             (der_fn must be dyadic)
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
 -    => dfn_decl                            (if dfn_decl is op)
 -    => (ID ← dfn_decl)                     (if dfn_decl is op)
 -    => op_or_fn
 -
 - fn => = ≤ < > ≥ ∨ ∧ ⍲ ⍱ ⍷ ∩ ←             (dyadic)
 -    => + - × ÷ * ⍟ ⌹ ○ ! ? | ⌈ ⌊ ⊥ ⊤       (monadic or dyadic)
 -    => ⊣ ⊢ ≠ ≡ ≢ ↑ ↓ ⊂ ⊃ ⊆ ⌷ ⍋ ⍒ ⍳ ⍸       (monadic or dyadic)
 -    => ∊ ∪ ~ , ⍪ ⍴ ⌽ ⊖ ⍉ ⍎ ⍕               (monadic or dyadic)
 -    => ⊃[der_arr]                          (monadic)
 -    => ⊆[der_arr]                          (dyadic)
 -    => ⌷[da] ⌽[da] ⊖[da] ,[da] ⍪[da]       (monadic or dyadic) (da = der_arr)
 -    => ↓[da] ↑[da] \[da] /[da] ⊂[da]       (monadic or dyadic) (da = der_arr)
 -    => ⎕ID                                 (if ⎕ID is a d_fn)
 -    => ⍺⍺ | ⍵⍵ | ∇                         (if dfn_decl state matches these)
 -    => dfn_decl                            (if dfn_decl is fn)
 -    => (ID ← dfn_decl)                     (if dfn_decl is fn)
 -    => (train)
 -    => op_or_fn
 -
 - op_or_fn => / ⌿ \ ⍀                       (monadic operators / dyadic functions)
 -
 - arr => arr_comp[index_list] {arr_comp[index_list]}
 -
 - arr_comp => scalar {scalar}
 -          => STR
 -          => ID                            (if ID is arr)
 -          => ⎕ID                           (if ⎕ID is arr)
 -          => (der_arr)
 -          => ⍺ | ⍵                         (if ⍺/w is in namespace)
 -          => ⍺⍺ | ⍵⍵                       (if ⍺⍺/ww is in namespace and is array)
 -          => ⍬
 -
 - scalar => NUM
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

type MatchFn a = [Token] -> Maybe (a, [Token])

matchOne :: [MatchFn a] -> MatchFn a
-- return first successful match, else Nothing
matchOne fns toks = foldl try Nothing fns
    where try (Just x) _ = Just x -- already found match
          try Nothing f = f toks

matchAll :: [MatchFn a] -> [Token] -> Maybe ([a], [Token])
-- match every function in list (sequentially), returning their results, else Nothing
matchAll fns toks = chFst (reverse) . foldl try (Just ([], toks)) $ fns
    where try Nothing _ = Nothing
          try (Just (rs, ts)) f = case f ts of
              Just (r, ts') -> Just(r:rs, ts')
              _ -> Nothing

matchMax :: [MatchFn a] -> [Token] -> Maybe ([[a]], [Token])
-- match 0 or more repetitions of the entire function list
matchMax fns toks = case matchAll fns toks of
    Nothing -> Nothing
    Just (r, ts) -> case matchMax fns ts of
        Nothing -> Just ([r], ts)
        Just (rs, ts') -> Just (r:rs, ts')

matchAllThenMax :: [MatchFn a] -> [Token] -> Maybe([[a]], [Token])
-- match 1 or more repetitions of entire function list
matchAllThenMax fns = chFst (concat) . matchAll [
        chFst (:[]) . matchAll fns,
        matchMax fns
    ]

{- Tuple Matching (hard-coded) -}

matchT2 :: (MatchFn a, MatchFn b) -> MatchFn (a, b)
matchT2 (fa, fb) ts = case fa ts of
    Nothing -> Nothing
    Just (a, ts') -> case fb ts' of
        Nothing -> Nothing
        Just (b, ts'') -> Just ((a, b), ts'')

matchT3 :: (MatchFn a, MatchFn b, MatchFn c) -> MatchFn (a, b, c)
matchT3 (fa, fb, fc) ts = case fa ts of
    Nothing -> Nothing
    Just (a, ts') -> case fb ts' of
        Nothing -> Nothing
        Just (b, ts'') -> case fc ts'' of
            Nothing -> Nothing
            Just (c, ts''') -> Just ((a, b, c), ts''')

matchT4 :: (MatchFn a, MatchFn b, MatchFn c, MatchFn d) -> MatchFn (a, b, c, d)
matchT4 (fa, fb, fc, fd) ts = case fa ts of
    Nothing -> Nothing
    Just (a, ts') -> case fb ts' of
        Nothing -> Nothing
        Just (b, ts'') -> case fc ts'' of
            Nothing -> Nothing
            Just (c, ts''') -> case fd ts''' of
                Nothing -> Nothing
                Just (d, ts'''') -> Just ((a, b, c, d), ts'''')

matchT5 :: (MatchFn a, MatchFn b, MatchFn c, MatchFn d, MatchFn e) -> MatchFn (a, b, c, d, e)
matchT5 (fa, fb, fc, fd, fe) ts = case fa ts of
    Nothing -> Nothing
    Just (a, ts') -> case fb ts' of
        Nothing -> Nothing
        Just (b, ts'') -> case fc ts'' of
            Nothing -> Nothing
            Just (c, ts''') -> case fd ts''' of
                Nothing -> Nothing
                Just (d, ts'''') -> case fe ts'''' of
                    Nothing -> Nothing
                    Just (e, ts5) -> Just ((a, b, c, d, e), ts5)

matchCh :: Char -> [Token] -> Maybe (Char, [Token])
matchCh c (ChTok c':ts)
    | c == c' = Just (c, ts)
    | otherwise = Nothing
matchCh _ _ = Nothing

matchId :: [Token] -> Maybe (String, [Token])
matchId (IdTok s:ts) = Just (s, ts)
matchId _ = Nothing

matchStrLiteral :: [Token] -> Maybe (String, [Token])
matchStrLiteral (StrTok s:ts) = Just (s, ts)
matchStrLiteral _ = Nothing

matchNumLiteral :: [Token] -> Maybe (Double, [Token])
matchNumLiteral (NumTok n:ts) = Just (n, ts)
matchNumLiteral _ = Nothing

{- Data Types -}

data ExprResult = ResArr ArrTreeNode
                | ResFn FnTreeNode
                | ResOp Operator

{- Parsing Functions -}

parseStatement :: [Token] -> Maybe [ExprResult]
parseStatement toks = case (parseExpr toks) of
    Nothing -> Nothing
    Just (res, rest) -> if (length rest) == 0
                        then Just res
                        else Nothing

parseExpr :: [Token] -> Maybe ([ExprResult], [Token])
parseExpr _ = Nothing -- TODO

-- parseDfnDecl :: [Token] -> Maybe (???, [Token])
-- parseDfnExpr :: [Token] -> Maybe (???, [Token])

parseIdxList :: [Token] -> Maybe ([ArrTreeNode], [Token])
parseIdxList _ = Nothing -- TODO

parseDerArr :: [Token] -> Maybe (ArrTreeNode, [Token])
parseDerArr _ = Nothing -- TODO

parseTrain :: [Token] -> Maybe (FnTreeNode, [Token])
parseTrain _ = Nothing -- TODO

parseDerFn :: [Token] -> Maybe (FnTreeNode, [Token])
parseDerFn _ = Nothing -- TODO

parseOp :: [Token] -> Maybe (Operator, [Token])
parseOp _ = Nothing -- TODO

parseFn :: [Token] -> Maybe (Function, [Token])
parseFn _ = Nothing -- TODO

parseOpOrFn :: [Token] -> Maybe ((Operator, Function), [Token])
parseOpOrFn _ = Nothing -- TODO

parseArr :: [Token] -> Maybe (ArrTreeNode, [Token]) -- parse an entire literal array
parseArr = chFst (squeezeNodes . concat) . matchAllThenMax [
        matchOne [
            chFst (setSubscript) . matchT4 (
                parseArrComp,
                matchCh '[',
                parseIdxList,
                matchCh ']'
            ),
            parseArrComp
        ]
    ]
        where squeezeNodes (a:[]) = a
              squeezeNodes as = ArrLeaf . arrFromList . map (scalarify) $ as
                where scalarify (ArrLeaf a@(Array [1] _)) = a `at` 0 -- don't box scalar
                      scalarify a = ScalarArr a
              setSubscript (lhs, _, ss, _) = mkDyadFnCall (FnLeaf fAssign) lhs (squeezeNodes ss)

parseArrComp :: [Token] -> Maybe (ArrTreeNode, [Token]) -- parse array "component"
parseArrComp = matchOne [
            -- scalar {scalar}
            chFst (ArrLeaf . arrFromList . concat) . matchAllThenMax [parseScalar],
            -- STR
            chFst (ArrLeaf . arrFromList . map ScalarCh) . matchStrLiteral,
            -- ID
            -- TODO (where ID is arr)
            -- ⎕ID
            -- TODO (where ⎕ID is arr)
            -- ⍺ | ⍵
            -- TODO (where ⍺ or ⍵ is in namespace)
            -- ⍺⍺ | ⍵⍵
            -- TODO (where ⍺⍺ or ⍵⍵ is in namespace and is array)
            -- ⍬
            chFst (\_ -> (ArrLeaf . arrFromList) []) . matchCh '⍬',
            -- (der_arr)
            chFst (\(_, da, _) -> da) . matchT3 (
                matchCh '(',
                parseDerArr,
                matchCh ')'
            )
    ]

parseScalar :: [Token] -> Maybe (Scalar, [Token])
parseScalar = chFst (\n -> ScalarNum n) . matchNumLiteral -- NUM
