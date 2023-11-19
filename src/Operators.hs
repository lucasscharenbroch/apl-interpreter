module Operators where
import Eval
import GrammarTree

{- Helpers -}

getDyadFn :: FnTreeNode -> FuncD
getDyadFn f = case unwrapFunction f of
   (DyadFn _ x) -> x
   (MonDyadFn _ _ x) -> x
   _ -> undefined -- TODO throw exception (expected dyadic function)

getMonFn :: FnTreeNode -> FuncM
getMonFn f = case unwrapFunction f of
    (MonFn _ x) -> x
    (MonDyadFn _ x _) -> x
    _ -> undefined -- TODO throw exception (expected monadic function)

{- General Operators -}

selfie :: FnTreeNode -> Function
selfie ft = case ft of
    (FnLeafArr at) -> MonDyadFn "der⍨" (\i _ -> evalArrTree i at) (\i _ _ -> evalArrTree i at)
    _ -> MonDyadFn "der⍨" (\i a -> dyFn i a a) (\i l r -> dyFn i r l)
        where dyFn = getDyadFn ft

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (MonDyadFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
