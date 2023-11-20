module Operators where
import Eval
import GrammarTree

{- Helpers -}

getDyadFn :: Function -> FuncD
getDyadFn f = case f of
   (DyadFn _ x) -> x
   (MonDyadFn _ _ x) -> x
   _ -> undefined -- TODO throw exception (expected dyadic function)

getMonFn :: Function -> FuncM
getMonFn f = case f of
    (MonFn _ x) -> x
    (MonDyadFn _ x _) -> x
    _ -> undefined -- TODO throw exception (expected monadic function)

{- Special Operators -}

{-
wrapOpAssignment :: String -> Operator -> Operator
wrapOpAssignment id op = case op of
    MonOp name m -> MonOp name (\i a -> )
    DyadOp name d ->
    -- DfnOp toks -> -- TODO
    -- -}

{- General Operators -}

{- Operators that sometimes take Arrays -}

selfie :: Either Function Array -> Function
selfie arg = case arg of
    (Left f) -> MonDyadFn "der⍨" (\i a -> dyFn i a a) (\i l r -> dyFn i r l)
        where dyFn = getDyadFn f
    (Right a) -> MonDyadFn "der⍨" (\i _ -> (i, a)) (\i _ _ -> (i, a))

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (MonDyadFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
