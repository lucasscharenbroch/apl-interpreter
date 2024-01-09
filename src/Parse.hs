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
import QuadNames

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
 -         => der_fn der_arr
 -         => arr der_fn der_arr
 -         => arr
 -
 - arr_ass => ID ← der_arr
 -         => ID der_fn ← der_arr
 -         => ⎕ID ← der_arr
 -         => ⎕ ← der_arr
 -
 - train => [df] {(arr|df) df} df            (where df = der_fn)
 -
 - der_fn => (f|a) op_like [f|a] {op_like [f|a]}       (where (f|a) is fn or arr; match the
 -                                                      optional iff op_like is dyadic)
 -                                                     (∘ may match (f|a) if the following op is .
 -                                                      (this allows for outer product))
 -                                                     (∘ must not match op if . follows it)
 -        => fn
 -
 - op_like => [der_arr]
 -         => op
 -         => op_or_fn
 -
 - op => ¨ ⍨ ⌸ ⌶                             (monadic)
 -    => ⍣ . ∘ ⍤ ⍥ @ ⍠ ⌺                     (dyadic)
 -    => dfn_decl                            (if dfn_decl is op)
 -    => ∇∇                                  (if ∇∇ is in id map)
 -    => op_ass
 -    => ⎕ ← op
 -    => ID                                  (if ID is op)
 -    => (op)
 -
 - op_ass => ID ← op
 -        => ⎕ ← op
 -
 - fn => = ≤ < > ≥ ∨ ∧ ⍲ ⍱ ⍷ ∩ ←             (dyadic)
 -    => + - × ÷ * ⍟ ⌹ ○ ! ? | ⌈ ⌊ ⊥ ⊤       (monadic or dyadic)
 -    => ⊣ ⊢ ≠ ≡ ≢ ↑ ↓ ⊂ ⊃ ⊆ ⌷ ⍋ ⍒ ⍳ ⍸       (monadic or dyadic)
 -    => ∊ ∪ ~ , ⍪ ⍴ ⌽ ⊖ ⍉ ⍎ ⍕               (monadic or dyadic)
 -    => ⍺⍺ | ⍵⍵ | ∇                         (if dfn_decl state matches these)
 -    => dfn_decl                            (if dfn_decl is fn)
 -    => fn_ass
 -    => ID                                  (if ID is fn)
 -    => op_or_fn
 -    => (train)
 -
 - fn_ass => ID ← train
 -        => ⎕ ← train
 -
 - op_or_fn => / ⌿ \ ⍀                       (monadic operators / dyadic functions)
 -
 - arr => arr_comp {arr_comp | [index_list]}
 -
 - arr_comp => scalar {scalar}
 -
 - scalar => NUM
 -        => STR
 -        => ⍞
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
   maybeMatch f >>= \mb -> case mb of
        Nothing -> matchOne fs
        Just x -> return x

matchAll :: [MatchFn a] -> MatchFn [a]
matchAll [] = return []
matchAll (f:fs) = (:) <$> f <*> matchAll fs

matchMax :: [MatchFn a] -> MatchFn [[a]]
-- match 0 or more repetitions of the entire function list
matchMax fns = do
    (maybeMatch $ matchAll fns) >>= \mb -> case mb of
        Nothing -> return []
        Just x -> do
            xs <- matchMax fns
            return $ x:xs

matchAllThenMax :: [MatchFn a] -> MatchFn [[a]]
-- match 1 or more repetitions of entire function list
matchAllThenMax fns = (:) <$> matchAll fns <*> matchMax fns

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
matchComment = matchCh '⍝' *> matchMax [matchAnyTokenExcept [ChTok '\n']] *> matchEoe

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
matchIdWith f = matchId >>= \s -> mapGetIf s f

matchSpecialIdWith :: Token -> String -> (IdEntry -> Maybe a) -> MatchFn a
matchSpecialIdWith specTok s f = do
    matchA $ \t -> if t == specTok then Just () else Nothing
    mapGetIf s f

matchAnyTokenExcept :: [Token] -> MatchFn Token
matchAnyTokenExcept blacklist = matchA $ \t -> if t `elem` blacklist then Nothing else Just t

{- Data Types -}

data ExprResult = ResAtn ArrTreeNode Bool -- Bool = should print?
                | ResFtn FnTreeNode Bool
                | ResOtn OpTreeNode Bool
                | ResNull

instance Show ExprResult where
    show (ResAtn a _) = show a
    show (ResFtn f _) = show f
    show (ResOtn o _) = show o

data DfnExprResult = DResAtn ArrTreeNode Bool -- Bool = should return?
                   | DResCond ArrTreeNode ArrTreeNode
                   | DResDefaultAlpha ArrTreeNode
                   | DResFtn FnTreeNode -- no bool: can't return function (should be an assignment)
                   | DResOtn OpTreeNode -- nor operator                   (should be an assignment)
                   | DResNull

data ParentheticalResult = ParenFtn FnTreeNode
                         | ParenAtn ArrTreeNode
                         | ParenOtn OpTreeNode

data OpLike = OpLikeM (FnTreeNode -> FnTreeNode)
            | OpLikeD (FnTreeNode -> FnTreeNode -> FnTreeNode)

{- Parsing Functions -}

parseExpr :: MatchFn ExprResult
parseExpr = matchOne [
        mkResAtn <$> parseDerArr <* matchCommentOrEoe,
        mkResFtn <$> parseTrain <* matchCommentOrEoe,
        mkResOtn <$> parseOp <* matchCommentOrEoe,
        (\_ -> ResNull) <$> matchCommentOrEoe
    ]
    where mkResAtn a@(ArrInternalAssignment _ _) = ResAtn a False
          mkResAtn a@(ArrInternalModAssignment _ _ _) = ResAtn a False
          mkResAtn a@(ArrInternalQuadAssignment _) = ResAtn a False
          mkResAtn a@(ArrInternalQuadIdAssignment _ _) = ResAtn a False
          mkResAtn a = ResAtn a True
          mkResFtn f@(FnInternalAssignment _ _) = ResFtn f False
          mkResFtn f@(FnInternalQuadAssignment _) = ResFtn f False
          mkResFtn f = ResFtn f True
          mkResOtn o@(OpInternalAssignment _ _) = ResOtn o False
          mkResOtn o@(OpInternalQuadAssignment _) = ResOtn o False
          mkResOtn o = ResOtn o True

parseParenthetical :: MatchFn ParentheticalResult
parseParenthetical = matchCh '(' *> matchOne [
        parseParenthetical <* matchCh ')',
        ParenAtn <$> parseDerArr <* matchCh ')',
        ParenFtn <$> parseTrain <* matchCh ')',
        ParenOtn <$> parseOp <* matchCh ')'
    ]

parseParentheticalAtn :: MatchFn ArrTreeNode
parseParentheticalAtn = parseParenthetical >>= \res -> case res of
    ParenAtn atn -> return atn
    _ -> mzero

parseParentheticalFtn :: MatchFn FnTreeNode
parseParentheticalFtn = parseParenthetical >>= \res -> case res of
    ParenFtn ftn -> return ftn
    _ -> mzero

parseParentheticalOtn :: MatchFn OpTreeNode
parseParentheticalOtn = parseParenthetical >>= \res -> case res of
    ParenOtn otn -> return otn
    _ -> mzero

parseDfnExpr :: MatchFn DfnExprResult
parseDfnExpr = matchOne [
        mkDResAtn <$> parseDerArr <* matchCommentOrEoe,
        (\(a1, a2) -> DResCond a1 a2) <$> parseGuard <* matchCommentOrEoe,
        DResDefaultAlpha <$> parseAlphaAss <* matchCommentOrEoe,
        DResFtn <$> parseFnAss <* matchCommentOrEoe,
        DResOtn <$> parseOpAss <* matchCommentOrEoe,
        (\_ -> DResNull) <$> matchCommentOrEoe
    ]
    where mkDResAtn a@(ArrInternalAssignment _ _) = DResAtn a False
          mkDResAtn a@(ArrInternalModAssignment _ _ _) = DResAtn a False
          mkDResAtn a = DResAtn a True

parseGuard :: MatchFn (ArrTreeNode, ArrTreeNode)
parseGuard = (,) <$> parseDerArr <*> (matchCh ':' *> parseDerArr)

parseAlphaAss :: MatchFn ArrTreeNode
parseAlphaAss = matchCh '⍺' *> matchCh '←' *> parseDerArr

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
    return (toks, AATok `elem` toks || WWTok `elem` toks, WWTok `elem` toks)

parseIdxList :: MatchFn [Maybe ArrTreeNode]
parseIdxList = (foldIdxList . concat) <$> matchMax [
        matchOne [
            (Left) <$> matchCh ';',
            (Right) <$> parseDerArr
        ]
    ]
    where emptyArr = ArrLeaf . listToArr $ []
          foldIdxList [] = [Nothing]
          foldIdxList (Right arr:[]) = [Just arr]
          foldIdxList (Left _:rest) = Nothing : foldIdxList rest
          foldIdxList (Right arr:Left _:rest) = (Just arr) : foldIdxList rest

parseDerArr :: MatchFn ArrTreeNode
parseDerArr = matchOne [
        parseArrAss,
        ArrInternalMonFn <$> parseDerFn <*> parseDerArr,
        (flip ArrInternalDyadFn) <$> parseArr <*> parseDerFn <*> parseDerArr,
        parseArr
    ]

parseArrAss :: MatchFn ArrTreeNode
parseArrAss = matchOne [
        ArrInternalAssignment <$> matchId <*> (matchCh '←' *> parseDerArr), -- ID ← da
        ArrInternalModAssignment <$> matchId <*> parseDerFn <*> (matchCh '←' *> parseDerArr), -- ID df ← da
        ArrInternalQuadIdAssignment <$> (matchCh '⎕' *> matchId) <*> (matchCh '←' *> parseDerArr), -- ⎕ID ← der_arr
        ArrInternalQuadAssignment <$> (matchCh '⎕' *> matchCh '←' *> parseDerArr) -- ⎕ ← der_arr
    ]

parseTrain :: MatchFn FnTreeNode
parseTrain = (tranify) <$> matchOne [(:) <$> parseDerFn <*> _matchTail, _matchTail]
    where _matchTail = do -- {(arr|df) df} df
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
    where _parseOpExpr = do arg <- _parseArg
                            opLike <- parseOpLike (Just arg)
                            _parseOpExprRec arg opLike
          _parseArg = matchOne [parseFn, FnLeafArr <$> parseArr]
          _parseOpExprRec :: FnTreeNode -> OpLike -> MatchFn FnTreeNode
          _parseOpExprRec lhs opLike = do
              df <- case opLike of
                        (OpLikeM f) -> return $ f lhs
                        (OpLikeD f) -> do rhs <- _parseArg
                                          return $ f lhs rhs
              maybeMatch (parseOpLike Nothing) >>= \mb -> case mb of
                   Nothing -> return $ df
                   (Just opLike2) -> _parseOpExprRec df opLike2

parseOpLike :: Maybe FnTreeNode -> MatchFn OpLike
parseOpLike mbftn = case mbftn of
        Just (FnLeafArr _) -> matchOne nonOverloadedOpMatchFns -- don't match overloaded glyphs
        _ -> matchOne allOpMatchFns                            -- if lhs is an array
    where nonOverloadedOpMatchFns = [
                  mkOpLike <$> parseOp,
                  OpLikeM . (flip FnInternalAxisSpec) <$> (matchCh '[' *> parseDerArr <* matchCh ']')
              ]
          allOpMatchFns = nonOverloadedOpMatchFns ++ [mkOpLike . OpLeaf . fst <$> parseOpOrFn]
          mkOpLike otn = case unwrapOpTree otn of
              MonOp _ _ -> OpLikeM $ FnInternalMonOp otn
              DyadOp _ _ -> OpLikeD $ FnInternalDyadOp otn

parseOp :: MatchFn OpTreeNode
parseOp = matchOne [
        matchOne $ map (\(c, o) -> (\_ -> OpLeaf o) <$> matchCh c) operatorGlyphs,
        -- dfn_decl
        dfnDeclToOp =<< parseDfnDecl,
        -- ∇∇
        matchSpecialIdWith DDTok "∇∇" idEntryToOtn,
        parseOpAss,
        matchIdWith (idEntryToOtn),
        OpInternalDummyNode <$> parseParentheticalOtn
    ]
    where idEntryToOtn (IdOp o) = Just (OpLeaf o)
          idEntryToOtn (IdDop toks is_dy) = Just . OpLeaf $ mkDop toks is_dy
          idEntryToOtn _ = Nothing
          dfnDeclToOp (toks, True, is_dy) = return . OpLeaf $ mkDop toks is_dy
          dfnDeclToOp _ = mzero

parseOpAss :: MatchFn OpTreeNode
parseOpAss = matchOne [
        OpInternalAssignment <$> matchId <*> (matchCh '←' *> parseOp), -- ID ← op
        OpInternalQuadAssignment <$> (matchCh '⎕' *> matchCh '←' *> parseOp) -- ⎕ ← op
    ]

parseFn :: MatchFn FnTreeNode
parseFn = matchOne [
        matchOne $ map (\(c, f) -> (\_ -> FnLeafFn f) <$> matchCh c) functionGlyphs,
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
        FnInternalDummyNode <$> parseParentheticalFtn
    ]
    where idEntryToFnTree (IdFn f) = Just $ FnLeafFn f
          idEntryToFnTree (IdDfn toks) = Just . FnLeafFn $ mkDfn toks Nothing Nothing Nothing
          idEntryToFnTree (IdDerDfn toks aa ww dd) = Just . FnLeafFn $ mkDfn toks aa ww dd
          idEntryToFnTree _ = Nothing
          dfnDeclToFn (toks, False, False) = return . FnLeafFn $ mkDfn toks Nothing Nothing Nothing
          dfnDeclToFn _ = mzero

parseFnAss :: MatchFn FnTreeNode
parseFnAss = matchOne [
        FnInternalAssignment <$> matchId <*> (matchCh '←' *> parseTrain), --  ID ← train
        FnInternalQuadAssignment <$> (matchCh '⎕' *> matchCh '←' *> parseTrain) -- ⎕ ← train
    ]

parseOpOrFn :: MatchFn (Operator, Function)
parseOpOrFn = matchOne $ map (\(c, o, f) -> (\_ -> (o, f)) <$> matchCh c) opOrFnGlyphs

parseArr :: MatchFn ArrTreeNode -- parse an entire literal array
parseArr = foldl (_merge) <$> parseArrComp <*> _parseComps
    where _merge a (Right i) = ArrInternalSubscript a i
          _merge a (Left a2) = ArrInternalImplCat a a2
          _parseComps = (concat) <$> matchMax [matchOne [
                  Left <$> parseArrComp,
                  Right <$> (matchCh '[' *> parseIdxList <* matchCh ']')
              ]]

parseArrComp :: MatchFn ArrTreeNode -- parse array "component"
parseArrComp = (applyImplCat . concat) <$> matchAllThenMax [parseScalar]
    where applyImplCat as@(_:_) = foldr (ArrInternalImplCat) (last as) (init as)

parseScalar :: MatchFn ArrTreeNode
parseScalar = matchOne [
            -- NUM
            (\n -> ArrLeaf . listToArr . (:[]) . ScalarNum $ n) <$> matchNumLiteral,
            -- STR
            (toScalarStr . map ScalarCh) <$> matchStrLiteral,
            -- ⍞
            (\_ -> implGroup $ ArrNiladicFn "⍞" fGetString) <$> matchCh '⍞',
            -- ID
            implGroup <$> matchIdWith (idEntryToArrTree),
            -- ⎕ID
            (matchCh '⎕' *> matchId) >>= \id -> return . implGroup $ ArrNiladicFn ("⎕" ++ id) (qget . getQuadName $ id),
            -- ⍺ | ⍵
            matchSpecialIdWith (ChTok '⍺') "⍺" (idEntryToArrTree),
            matchSpecialIdWith (ChTok '⍵') "⍵" (idEntryToArrTree),
            -- ⍺⍺ | ⍵⍵
            matchSpecialIdWith AATok "⍺⍺" (idEntryToArrTree),
            matchSpecialIdWith WWTok "⍵⍵" (idEntryToArrTree),
            -- ⍬
            (\_ -> implGroup $ (ArrLeaf . listToArr) []) <$> matchCh '⍬',
            -- (der_arr)
            implGroup <$> parseParentheticalAtn
        ]
    where toScalarStr (c:[]) = ArrLeaf . listToArr $ [c]
          toScalarStr s = implGroup (ArrLeaf . listToArr $ s)
          idEntryToArrTree (IdArr a) = Just $ ArrLeaf a
          idEntryToArrTree _ = Nothing
          implGroup = ArrInternalMonFn (FnLeafFn fImplicitGroup)
