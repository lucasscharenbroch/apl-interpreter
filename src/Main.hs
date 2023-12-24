import System.Console.Haskeline
import GlyphCompletion
import Lex (Token(..), tokenize, countBracketNesting)
import Parse
import Eval
import GrammarTree
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (intersperse)
import System.Exit

{- Global State -}

data GlobalState = GlobalState {
       verbosity :: Bool -- display trees before evaluation?
     }

defaultGlobalState = GlobalState False

{- Option Parsing -}

usageHeader = "Usage: ai [options]"

data CliOption = VerboseFlag | HelpFlag

optDescriptions :: [OptDescr CliOption]
optDescriptions = [
        Option "v" ["verbose"] (NoArg VerboseFlag) "print array/function/operator trees before evaluation",
        Option "h" ["help"] (NoArg HelpFlag) "print help information"
    ]

parseArgv :: IO [CliOption]
parseArgv = do
    argv <- getArgs
    case getOpt Permute optDescriptions argv of
        (opts, [], []) -> return opts
        (opts, nonOptions, []) -> ioError . userError $ errStr
            where errStr = "\nUnexpected argument(s): " ++ (concat . intersperse "; ") nonOptions ++ "\n" ++ usageInfo usageHeader optDescriptions
        (opts, _, errs) -> ioError . userError $ errStr
            where errStr = "\nInvalid argument(s): " ++ concat errs ++ usageInfo usageHeader optDescriptions

handleArgv :: [CliOption] -> IO GlobalState
handleArgv [] = return defaultGlobalState
handleArgv (HelpFlag:opts) = do
    putStr $ usageInfo usageHeader optDescriptions
    handleArgv opts
    exitWith ExitSuccess
handleArgv (VerboseFlag:opts) = do
    handleArgv opts >>= \x -> return $ x { verbosity = True }

{- Constants -}

defaultIdMap = mapInsert "⎕IO" (IdArr . arrFromList $ [ScalarNum 1]) emptyIdMap

{- Helpers -}

lift2 = lift . lift

evalArrTree' :: IdMap -> ArrTreeNode -> IO (IdMap, Array)
evalArrTree' idm atn = (\(r, i) -> (i, r)) <$> runStateT (evalArrTree atn) idm

evalFnTree' :: IdMap -> FnTreeNode -> IO (IdMap, Function)
evalFnTree' idm ftn = (\(r, i) -> (i, expectFunc r)) <$> runStateT (evalFnTree ftn) idm

evalOpTree' :: IdMap -> OpTreeNode -> IO (IdMap, Operator)
evalOpTree' idm otn = (\(r, i) -> (i, r)) <$> runStateT (evalOpTree otn) idm

handleRes :: IdMap -> ExprResult -> ReaderT GlobalState (InputT IO) IdMap
handleRes idMap x = case x of
    (ResAtn a True) -> do (i, a') <- lift2 $ evalArrTree' idMap a
                          showIfVerbose a
                          lift $ outputStrLn . show $ a'
                          return i
    (ResAtn a False) -> do (i, _) <- lift2 $ evalArrTree' idMap a
                           showIfVerbose a
                           return i
    (ResFtn f True) -> do (i, f') <- lift2 $ evalFnTree' idMap f
                          showIfVerbose f
                          lift $ outputStrLn . show $ f'
                          return i
    (ResFtn f False) -> do (i, _) <- lift2 $ evalFnTree' idMap f
                           showIfVerbose f
                           return i
    (ResOp o True) -> do (i, o') <- lift2 $ evalOpTree' idMap o
                         showIfVerbose o
                         lift $ outputStrLn . show $ o'
                         return i
    (ResOp o False) -> do (i, o') <- lift2 $ evalOpTree' idMap o
                          showIfVerbose o
                          return i
    (ResNull) -> return idMap
    where showIfVerbose :: (Show a) => a -> ReaderT GlobalState (InputT IO) ()
          showIfVerbose x = ask >>= \state -> case verbosity state of
              True -> lift $ outputStrLn . show $ x
              False -> return ()

execStatement :: IdMap -> [Token] -> ReaderT GlobalState (InputT IO) IdMap
execStatement idm [] = return idm
execStatement idm ts = case evalMatchFn idm ts parseExpr of
    Nothing -> do lift $ outputStrLn "parse error" -- TODO syntax error
                  return idm
    Just (res, ts') -> do idm' <- handleRes idm res
                          execStatement idm' ts'

mainloop :: IdMap -> [Token] -> ReaderT GlobalState (InputT IO) ()
mainloop idMap carriedToks = do
    isInterractive <- lift $ haveTerminalUI
    input <- lift $ getInputLine $ if isInterractive then ps else ""
    case input of
        Nothing -> return ()
        Just s -> do let toks = carriedToks ++ tokenize s
                     let nestingLevel' = countBracketNesting toks
                     let (mIdMap', carriedToks')  = if nestingLevel' <= 0 then (execStatement idMap toks, [])
                                                    else (return idMap, toks ++ [ChTok '\n'])
                     idMap' <- mIdMap'
                     mainloop idMap' carriedToks'
    where nestingLevel = countBracketNesting carriedToks
          ps = if nestingLevel == 0 then "    "
               else (concat $ replicate nestingLevel "    ") ++ ">   "

main :: IO ()
main = do
    gState <- parseArgv >>= handleArgv
    runInputT settings $ (flip runReaderT) gState $ (mainloop defaultIdMap [])
    where settings = setComplete completeGlyph defaultSettings
