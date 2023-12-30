module Operators where
import Eval
import GrammarTree
import PrettyPrint
import qualified Functions as F

{- Helpers -}

autoInfoMonFnM :: ShowAndPad a => String -> a -> FuncM -> Function
autoInfoMonFnM s a = MonFn (namePadToFnInfoM $ showMonTreeHelper (showAndPad a) s)

autoInfoDyadFnM :: ShowAndPad a => String -> a -> FuncD -> Function
autoInfoDyadFnM s a = DyadFn (namePadToFnInfoD $ showMonTreeHelper (showAndPad a) s)

autoInfoMonDyadFnM :: ShowAndPad a => String -> a -> FuncM -> FuncD -> Function
autoInfoMonDyadFnM s a = MonDyadFn (namePadToFnInfoA $ showMonTreeHelper (showAndPad a) s)

autoInfoMonFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncM -> Function
autoInfoMonFnD s a b = MonFn (namePadToFnInfoM $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

autoInfoDyadFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncD -> Function
autoInfoDyadFnD s a b = DyadFn (namePadToFnInfoD $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

autoInfoMonDyadFnD :: (ShowAndPad a, ShowAndPad b) => String -> a -> b -> FuncM -> FuncD -> Function
autoInfoMonDyadFnD s a b = MonDyadFn (namePadToFnInfoA $ showDyadTreeHelper (showAndPad a) (showAndPad b) s)

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

selfie :: Either Array Function -> Function
selfie arg = case arg of
    (Left a) -> autoInfoMonDyadFnM "⍨" arg (\_ -> return a) (\_ _ -> return a)
    (Right f) -> autoInfoMonDyadFnM "⍨" arg (\a -> dyFn a a) (\l r -> dyFn r l)
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
