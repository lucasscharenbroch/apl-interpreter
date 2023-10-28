module Parse where
import Lex (Token)
import GrammarTree

{-
 -
 - statement => expr <EOS>                   (EOS is end of statement: no tokens left)
 -
 - expr => ⎕ ← der_arr
 -      => der_arr | train | op
 -      => assignment
 -      => [expr] ⍝ <ignore-until-eol> (don't recursively match expr: check after above match is found)
 -      => expr {⋄ expr} [⋄]           (don't recursively match expr: check after above match is found)
 -
 - assignment => der_arr ← der_arr | train | op
 -
 - dfn_decl => { dfn_expr }
 -
 - dfn_expr => assignment
 -          => der_arr | train | op
 -          => der_arr : der_arr
 -          => [dfn_expr] ⍝ <discard-until-newline-or-⋄>
 -          => dfn_expr {⋄ dfn_expr} [⋄]
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
 -           => fn
 -
 - op => ¨ ⍨ ⌸ ⌶                             (monadic)
 -    => ⍣ . ∘ ⍤ ⍥ @ ⍠ ⌺                     (dyadic)
 -    => assignment                          (if assignment is op)
 -    => dfn_decl                            (if dfn_decl is op)
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
 -    => (train)
 -    => op_or_fn
 -
 - op_or_fn => / ⌿ \ ⍀                       (monadic operators / dyadic functions)
 -
 - arr => scalar {scalar}
 -     => ID                                 (if ID is arr)
 -     => ⎕ID                                (if ⎕ID is arr)
 -     => (assignment)                       (if assignment is arr)
 -     => (der_arr)
 -     => ⍺ | ⍵                              (if dfn_decl state matches these)
 -     => ⍬
 -     => arr[index_list]
 -     => arr arr {arr}
 -
 - index_list => (der_arr|;)*
 -
 - scalar => NUM
 -        => STR
 -        => ID                             (if ID is a scalar)
 -
 - Tokens:
 - NUM: ¯?[0-9]*\.?[0-9]+
 - STR: '([^']|'')*'
 - ID: [a-zA-Z_][a-zA-Z_0-9]*
 - (all other non-whitespace tokens are read literally)
 -
 -}

data ExprResult = ResArr Array
                | ResFn Function
                | ResOp Operator

parseStatement :: [Token] -> Maybe [ExprResult]
parseStatement toks = case (parseExpr toks) of
    Nothing -> Nothing
    Just (res, rest) -> if (length rest) == 0
                        then Just res
                        else Nothing

parseExpr :: [Token] -> Maybe ([ExprResult], [Token])
parseExpr _ = Nothing

parseAssignment :: [Token] -> Maybe (ArrTreeNode, [Token])
parseAssignment _ = Nothing

-- parseDfnDecl :: [Token] -> Maybe (???, [Token])
-- parseDfnExpr :: [Token] -> Maybe (???, [Token])

parseDerArr :: [Token] -> Maybe (ArrTreeNode, [Token])
parseDerArr _ = Nothing

parseTrain :: [Token] -> Maybe (FnTreeNode, [Token])
parseTrain _ = Nothing

parseDerFn :: [Token] -> Maybe (FnTreeNode, [Token])
parseDerFn _ = Nothing

parseOp :: [Token] -> Maybe (Operator, [Token])
parseOp _ = Nothing

parseFn :: [Token] -> Maybe (Function, [Token])
parseFn _ = Nothing

parseOpOrFn :: [Token] -> Maybe ((Operator, Function), [Token])
parseOpOrFn _ = Nothing

parseArr :: [Token] -> Maybe (Array, [Token])
parseArr _ = Nothing

parseIdxList :: [Token] -> Maybe ([ArrTreeNode], [Token])
parseIdxList _ = Nothing

parseScalar :: [Token] -> Maybe (Scalar, [Token])
parseScalar _ = Nothing
