module Parse where
import Lex (Token(..))
import GrammarTree

{-
 -
 - statement => expr <EOS>                   (EOS is end of statement: no tokens left)
 -
 - expr => ⎕ ← der_arr
 -      => [der_arr ←] der_arr | train | op
 -      => [expr] ⍝ <ignore-until-eol> (don't recursively match expr: check after above match is found)
 -      => expr {⋄ expr} [⋄]           (don't recursively match expr: check after above match is found)
 -
 - dfn_decl => { dfn_expr }
 -
 - dfn_expr => [der_arr ←] der_arr | train | op
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
 - fn => = ≤ < > ≥ ∨ ∧ ⍲ ⍱ ⍷ ∩               (dyadic)
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
 -          => (der_arr [← der_arr])
 -          => ⍺ | ⍵                         (if dfn_decl state matches these and ⍺/w is arr)
 -          => ⍬
 -
 - scalar => NUM
 -        => ID                              (if ID is a scalar)
 -        => ⍺ | ⍵                           (if dfn_decl state matches these and ⍺/⍵ is scalar)
 -
 - Tokens:
 - NUM: ¯?[0-9]*\.?[0-9]+
 - STR: '([^']|'')*'
 - ID: [a-zA-Z_][a-zA-Z_0-9]*
 - (all other non-whitespace tokens are read literally)
 -
 -}

{- Matching Functions -}

chFst :: (a -> a) -> Maybe (a, b) -> Maybe (a, b)
-- chain first: apply function to first element of maybe-wrapped tuple
chFst f m = case m of
    Nothing -> Nothing
    Just (x, y) -> Just (f x, y)

mchFst :: (a -> Maybe a) -> Maybe (a, b) -> Maybe (a, b)
-- maybe chain first
mchFst f m = case m of
    Nothing -> Nothing
    Just (x, y) -> case (f x) of
        Nothing -> Nothing
        Just z -> Just(z, y)

matchOne :: [[Token] -> Maybe (a, [Token])] -> [Token] -> Maybe (a, [Token])
-- return first successful match, else Nothing
matchOne fns toks = foldl try Nothing fns
    where try (Just x) _ = Just x -- already found match
          try Nothing f = f toks

matchAll :: [[Token] -> Maybe (a, [Token])] -> [Token] -> Maybe ([a], [Token])
-- match every function in list (sequentially), returning their results, else Nothing
matchAll fns toks = chFst (reverse) . foldl try (Just ([], toks)) $ fns
    where try Nothing _ = Nothing
          try (Just (rs, ts)) f = case f ts of
              Just (r, ts') -> Just(r:rs, ts')
              _ -> Nothing

matchMax :: [[Token] -> Maybe (a, [Token])] -> [Token] -> Maybe ([[a]], [Token])
-- match 0 or more repetitions of the entire function list
matchMax fns toks = case matchAll fns toks of
    Nothing -> Nothing
    Just (r, ts) -> case matchMax fns ts of
        Nothing -> Just ([r], ts)
        Just (rs, ts') -> Just (r:rs, ts')

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

parseArr :: [Token] -> Maybe (Array, [Token]) -- parse an entire literal array
parseArr _ = Nothing -- TODO

parseArrComp :: [Token] -> Maybe (Array, [Token]) -- parse array "component"
parseArrComp _ = Nothing

parseIdxList :: [Token] -> Maybe ([ArrTreeNode], [Token])
parseIdxList _ = Nothing -- TODO

parseScalar :: [Token] -> Maybe (Scalar, [Token])
parseScalar _ = Nothing -- TODO
