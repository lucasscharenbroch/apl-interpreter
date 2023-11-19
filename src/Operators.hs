module Operators where
import Eval
import GrammarTree

{- Helpers -}

getDyadFn :: Function -> FuncD
getDyadFn f = case f of
    (DyadFn _ x) -> x
    (MonDyadFn _ _ x) -> x
    _ -> undefined -- TODO throw exception

getMonFn :: Function -> FuncM
getMonFn f = case f of
    (MonFn _ x) -> x
    (MonDyadFn _ x _) -> x
    _ -> undefined -- TODO throw exception

{- General Operators -}

selfie :: OpM
selfie _ ft = case evalFnTree (idm, ft) of
    (Left f) -> MonDyadFn "der⍨" (\x -> dyFn x x) (\x y -> dyFn y x)
        where dyFn = getDyadFn f
    (Right a) -> MonDyadFn "der⍨" (\_ -> a) (\_ _ -> a)

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (MonDyadFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
