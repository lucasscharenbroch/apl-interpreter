module Operators where
import Eval
import GrammarTree
import PrettyPrint
import qualified Functions as F
import Control.Exception (throw)
import Exceptions
import Control.Monad

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
   _ -> throw $ SyntaxError "expected dyadic function"

getMonFn :: Function -> FuncM
getMonFn f = case f of
    (MonFn _ x) -> x
    (MonDyadFn _ x _) -> x
    _ -> throw $ SyntaxError "expected monadic function"

{- Special Operators -}

{- General Operators -}

over :: Function -> Function -> Function
over f g = case (f, g) of
    (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (MonFn _ fF, MonDyadFn _ gF _) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (DyadFn _ fF, MonDyadFn _ gF _) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (MonDyadFn _ fMF fDF, MonFn _ gF) -> autoInfoMonDyadFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    (MonDyadFn _ fMF fDF, MonDyadFn _ gF _) -> autoInfoMonDyadFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    _ -> throw . SyntaxError $ "(⍥): invalid arity of arguments"
    where overDM d m l r = join $ d <$> m l <*> m r

{- Operators that sometimes take Arrays -}

atop :: (Either Array Function) -> (Either Array Function) -> Function
atop l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (MonFn _ fF, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (MonFn _ fF, MonDyadFn _ gMF gDF) -> autoInfoMonDyadFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        (MonDyadFn _ fF _, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (MonDyadFn _ fF _, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (MonDyadFn _ fF _, MonDyadFn _ gMF gDF) -> autoInfoMonDyadFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        _ -> throw . SyntaxError $ "(⍤): invalid arity of argument functions"
    -- TODO rank
    _ -> throw . SyntaxError $ "(⍤): invalid argument types"

jot :: (Either Array Function) -> (Either Array Function) -> Function
jot l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (MonFn _ fF, MonDyadFn _ gF _) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (DyadFn _ fF, MonDyadFn _ gF _) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (MonDyadFn _ fMF fDF, MonFn _ gF) -> autoInfoMonDyadFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        (MonDyadFn _ fMF fDF, MonDyadFn _ gF _) -> autoInfoMonDyadFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        _ -> throw . SyntaxError $ "(∘): invalid arity of argument functions"
    (Right f, Left a) -> autoInfoMonFnD "∘" f a (curryRight (getDyadFn f) a)
    (Left a, Right f) -> autoInfoMonFnD "∘" a f (curryLeft a (getDyadFn f))
    _ -> throw . SyntaxError $ "(∘): invalid argument types"
    where besideDM d m l r = join $ d <$> return l <*> m r
          curryRight f a x = f x a
          curryLeft a f x = f a x

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
