module Operators where
import Eval
import GrammarTree

{- Reordered-Argument Function-Tree-Node Constructors -}

_MonFn :: FuncM -> String -> Function
_MonFn f n = MonFn n f

_DyadFn :: FuncD -> String -> Function
_DyadFn f n = DyadFn n f

_MonDyadFn :: FuncM -> FuncD -> String -> Function
_MonDyadFn m d n = MonDyadFn n m d

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

{- General Operators -}

{- Operators that sometimes take Arrays -}

selfie :: Either Array Function -> (String -> Function)
selfie arg = case arg of
    (Left a) -> _MonDyadFn (\_ -> return a) (\_ _ -> return a)
    (Right f) -> _MonDyadFn (\a -> dyFn a a) (\l r -> dyFn r l)
        where dyFn = getDyadFn f

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (MonDyadFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
