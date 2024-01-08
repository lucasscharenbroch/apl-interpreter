module Operators where
import Eval
import GrammarTree
import PrettyPrint
import qualified Functions as F
import Control.Exception (throw)
import Exceptions
import Control.Monad
import Data.Function
import Util
import Data.Maybe

{- Helpers -}

getDyadFn :: Function -> FuncD
getDyadFn f = case f of
   (DyadFn _ x) -> x
   (AmbivFn _ _ x) -> x
   _ -> throw $ SyntaxError "expected dyadic function"

getMonFn :: Function -> FuncM
getMonFn f = case f of
    (MonFn _ x) -> x
    (AmbivFn _ x _) -> x
    _ -> throw $ SyntaxError "expected monadic function"

getInverseM :: Function -> FuncM
getInverseM f = case f of
    MonFn i _ -> fromMaybe err (fnInverseM i)
    AmbivFn i _ _ -> fromMaybe err (fnInverseAM i)
    where err = throw . DomainError $ "function has no inverse: {" ++ (show f) ++ "}"

getInverseD :: Function -> FuncD
getInverseD f = case f of
    DyadFn i _ -> fromMaybe err (fnInverseD i)
    AmbivFn i _ _ -> fromMaybe err (fnInverseAD i)
    where err = throw . DomainError $ "function has no inverse: {" ++ (show f) ++ "}"

{- General Operators -}

each :: Function -> Function
each f = case f of
    MonFn _ fM -> autoInfoMonFnM "¨" f (_eachM fM)
    DyadFn _ fD -> autoInfoDyadFnM "¨" f (_eachD fD)
    AmbivFn _ fM fD -> autoInfoAmbivFnM "¨" f (_eachM fM) (_eachD fD)
    where _eachM :: FuncM -> FuncM
          _eachM m x = arrMapM ((arrToScalar<$>) . m . scalarToArr) $ x
          _eachD :: FuncD -> FuncD
          _eachD d x y = arrZipWithM ((arrToScalar<$>) .: on d scalarToArr) x' y'
              where (x', y') = rankMorph (x, y)
          scalarToArr (ScalarArr a) = a
          scalarToArr s = listToArr [s]
          arrToScalar a
              | arrNetSize a == 1 = head $ arrToList a
              | otherwise = ScalarArr a

over :: Function -> Function -> Function
over f g = case (f, g) of
    (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (MonFn _ fF, AmbivFn _ gF _) -> autoInfoMonFnD "⍥" f g (atopMM fF gF)
    (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (DyadFn _ fF, AmbivFn _ gF _) -> autoInfoDyadFnD "⍥" f g (overDM fF gF)
    (AmbivFn _ fMF fDF, MonFn _ gF) -> autoInfoAmbivFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    (AmbivFn _ fMF fDF, AmbivFn _ gF _) -> autoInfoAmbivFnD "⍥" f g (atopMM fMF gF) (overDM fDF gF)
    _ -> throw . SyntaxError $ "(⍥): invalid arity of arguments"
    where overDM d m l r = join $ d <$> m l <*> m r

{- Operators that sometimes take Arrays -}

atop :: (Either Array Function) -> (Either Array Function) -> Function
atop l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (MonFn _ fF, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (MonFn _ fF, AmbivFn _ gMF gDF) -> autoInfoAmbivFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        (AmbivFn _ fF _, MonFn _ gF) -> autoInfoMonFnD "⍤" l r (atopMM fF gF)
        (AmbivFn _ fF _, DyadFn _ gF) -> autoInfoDyadFnD "⍤" l r (atopMD fF gF)
        (AmbivFn _ fF _, AmbivFn _ gMF gDF) -> autoInfoAmbivFnD "⍤" l r (atopMM fF gMF) (atopMD fF gDF)
        _ -> throw . SyntaxError $ "(⍤): invalid arity of argument functions"
    -- TODO rank
    _ -> throw . SyntaxError $ "(⍤): invalid argument types"

jot :: (Either Array Function) -> (Either Array Function) -> Function
jot l r = case (l, r) of
    (Right f, Right g) -> case (f, g) of
        (MonFn _ fF, MonFn _ gF) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (MonFn _ fF, AmbivFn _ gF _) -> autoInfoMonFnD "∘" f g (atopMM fF gF)
        (DyadFn _ fF, MonFn _ gF) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (DyadFn _ fF, AmbivFn _ gF _) -> autoInfoDyadFnD "∘" f g (besideDM fF gF)
        (AmbivFn _ fMF fDF, MonFn _ gF) -> autoInfoAmbivFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        (AmbivFn _ fMF fDF, AmbivFn _ gF _) -> autoInfoAmbivFnD "∘" f g (atopMM fMF gF) (besideDM fDF gF)
        _ -> throw . SyntaxError $ "(∘): invalid arity of argument functions"
    (Right f, Left a) -> autoInfoMonFnD "∘" f a (curryRight (getDyadFn f) a)
    (Left a, Right f) -> autoInfoMonFnD "∘" a f (curryLeft a (getDyadFn f))
    _ -> throw . SyntaxError $ "(∘): invalid argument types"
    where besideDM d m l r = join $ d <$> return l <*> m r
          curryRight f a x = f x a
          curryLeft a f x = f a x

power :: (Either Array Function) -> (Either Array Function) -> Function
power l r = case (l, r) of
    (Right f, Left a) -> case f of
        (MonFn _ fM) -> autoInfoMonFnD "⍣" l r (_powM f')
                        where f' = if n < 0 then getInverseM f else fM
        (DyadFn _ fD) -> autoInfoDyadFnD "⍣" l r (_powD f')
                         where f' = if n < 0 then getInverseD f else fD
        (AmbivFn _ fM fD) -> autoInfoAmbivFnD "⍣" l r (_powM f'm) (_powD f'd)
                               where (f'm, f'd) = if n < 0 then (getInverseM f, getInverseD f) else (fM, fD)
        where n = arrToInt a
              _powM m x = foldM (flip ($)) x (replicate (abs n) m)
              _powD d x y = foldM (flip ($)) y (replicate (abs n) (d x))
    (Right f, Right g) -> case f of
        MonFn _ fM -> autoInfoMonFnD "⍣" l r (_powM fM)
        DyadFn _ fD -> autoInfoDyadFnD "⍣" l r (_powD fD)
        AmbivFn _ fM fD -> autoInfoAmbivFnD "⍣" l r (_powM fM) (_powD fD)
        where _powM :: FuncM -> FuncM
              _powM m y = do y' <- m y
                             stop <- arrToBool <$> g' y' y
                             if stop then return y' else _powM m y'
              _powD :: FuncD -> FuncD
              _powD d x y = do y' <- d x y
                               stop <- arrToBool <$> g' y' y
                               if stop then return y' else _powD d x y'
              g' = getDyadFn g
    _ -> throw . SyntaxError $ "(⍣): invalid argument types"

selfie :: Either Array Function -> Function
selfie arg = case arg of
    (Left a) -> autoInfoAmbivFnM "⍨" arg (\_ -> return a) (\_ _ -> return a)
    (Right f) -> autoInfoAmbivFnM "⍨" arg (\a -> dyFn a a) (\l r -> dyFn r l)
        where dyFn = getDyadFn f

{-
reduce :: FnTreeNode -> Function
reduce ft = MonFn "der/" -- TODO
    where f = case evalFnTree ft of
              (Left (DyadFn _ f)) -> f
              (Left (AmbivFn _ _ f)) -> f
              (Left _) -> undefined -- TODO exception: need dyadic function
              _ -> undefined
-}
