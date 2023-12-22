import System.Console.Haskeline
import GlyphCompletion
import Lex (Token(..), tokenize, countBracketNesting)
import Parse
import Eval
import GrammarTree
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

{- Constants -}

defaultIdMap = mapInsert "⎕IO" (IdArr . arrFromList $ [ScalarNum 1]) emptyIdMap

{- Helpers -}

evalArrTree' :: IdMap -> ArrTreeNode -> IO (IdMap, Array)
evalArrTree' idm atn = (\(r, i) -> (i, r)) <$> runStateT (evalArrTree atn) idm

evalFnTree' :: IdMap -> FnTreeNode -> IO (IdMap, Function)
evalFnTree' idm ftn = (\(r, i) -> (i, expectFunc r)) <$> runStateT (evalFnTree ftn) idm

evalOpTree' :: IdMap -> OpTreeNode -> IO (IdMap, Operator)
evalOpTree' idm otn = (\(r, i) -> (i, r)) <$> runStateT (evalOpTree otn) idm

handleRes :: IdMap -> ExprResult -> InputT IO IdMap
handleRes idMap x = case x of
    (ResAtn a True) -> do (i, a') <- lift $ evalArrTree' idMap a
                          outputStrLn . show $ a'
                          return i
    (ResAtn a False) -> do (i, _) <- lift $ evalArrTree' idMap a
                           return i
    (ResFtn f True) -> do (i, f') <- lift $ evalFnTree' idMap f
                          outputStrLn . show $ f'
                          return i
    (ResFtn f False) -> do (i, _) <- lift $ evalFnTree' idMap f
                           return i
    (ResOp o True) -> do (i, o') <- lift $ evalOpTree' idMap o
                         outputStrLn . show $ o'
                         return i
    (ResOp o False) -> do (i, o') <- lift $ evalOpTree' idMap o
                          return i
    (ResNull) -> return idMap

execStatement :: IdMap -> [Token] -> InputT IO IdMap
execStatement idm [] = return idm
execStatement idm ts = case evalMatchFn idm ts parseExpr of
    Nothing -> do outputStrLn "parse error" -- TODO syntax error
                  return idm
    Just (res, ts') -> do idm' <- handleRes idm res
                          execStatement idm' ts'

mainloop :: IdMap -> [Token] -> InputT IO ()
mainloop idMap carriedToks = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then ps else ""
    case input of
        Nothing -> return ()
        Just s -> do -- outputStrLn . show . tokenize $ s
                     let toks = carriedToks ++ tokenize s
                     let nestingLevel' = countBracketNesting toks
                     let (mIdMap', carriedToks')  = if nestingLevel' <= 0 then (execStatement idMap toks, [])
                                                    else (return idMap, toks ++ [ChTok '\n'])
                     idMap' <- mIdMap'
                     mainloop idMap' carriedToks'
    where nestingLevel = countBracketNesting carriedToks
          ps = if nestingLevel == 0 then "    "
               else (concat $ replicate nestingLevel "    ") ++ ">   "

main :: IO ()
main = runInputT settings (mainloop defaultIdMap [])
    where settings = setComplete completeGlyph defaultSettings
