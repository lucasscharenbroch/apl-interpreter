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
import Exceptions
import Control.Exception

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

showResIfVerbose :: ExprResult -> ReaderT GlobalState (InputT IO) ()
showResIfVerbose x = do
    ask >>= \state -> case verbosity state of
        False -> return ()
        True -> case x of
            ResNull -> return ()
            (ResAtn a _) -> lift $ outputStrLn . show $ a
            (ResFtn f _) -> lift $ outputStrLn . show $ f
            (ResOp o _) -> lift $ outputStrLn . show $ o

handleRes :: IdMap -> ExprResult -> ReaderT GlobalState (InputT IO) IdMap
handleRes idMap x = do
    showResIfVerbose x
    case x of
        (ResAtn a shouldShow) -> (lift2 . catchExecErr $ evalArrTree' idMap a) >>= \x -> case x of
            Just (i, a') -> showIf a' shouldShow >> return i
            Nothing -> return idMap
        (ResFtn f shouldShow) -> (lift2 $ catchExecErr $ evalFnTree' idMap f) >>= \x -> case x of
            Just (i, f') -> showIf f' shouldShow  >> return i
            Nothing -> return idMap
        (ResOp o shouldShow) -> (lift2 $ catchExecErr $ evalOpTree' idMap o) >>= \x -> case x of
            Just (i, o') -> showIf o' shouldShow >> return i
        (ResNull) -> return idMap
    where showIf :: Show a => a -> Bool -> ReaderT GlobalState (InputT IO) ()
          showIf a p = if p
                       then lift . outputStrLn $ show p
                       else return ()

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
