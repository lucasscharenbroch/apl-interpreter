module Operators where
import Eval
import GrammarTree
import PrettyPrint
import qualified Functions as F

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

deriveAmbivInfo :: (Either Array Function) -> (Either Array Function) -> String -> FnInfoA
deriveAmbivInfo arg1 arg2 name = namePadToFnInfoA $ showDyadTreeHelper (_show arg1) (_show arg2) name
    where _show arg = case arg of
              Left a -> (show a, 0)
              Right f -> fnToNameAndPad f


{- Special Operators -}

{- General Operators -}

{- Operators that sometimes take Arrays -}

selfie :: Either Array Function -> Function
selfie arg = case arg of
    (Left a) -> MonDyadFn infoA (\_ -> return a) (\_ _ -> return a)
    (Right f) -> MonDyadFn infoA (\a -> dyFn a a) (\l r -> dyFn r l)
        where dyFn = getDyadFn f
    where infoA = deriveAmbivInfo arg arg "⍨"

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (MonDyadFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
