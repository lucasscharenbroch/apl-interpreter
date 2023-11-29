import System.Console.Haskeline
import GlyphCompletion
import Lex (Token, tokenize)
import Parse
import Eval
import GrammarTree -- TODO remove

handleRes :: IdMap -> ExprResult -> (IdMap, InputT IO ())
handleRes idMap x = case x of
    (ResAtn a True) -> (i, outputStrLn $ show a')
      where (i, a') = evalArrTree idMap a
    (ResAtn a False) -> (i, return ())
      where (i, _) = evalArrTree idMap a
    (ResFtn f True) -> (i, outputStrLn . show $ f')
      where (i, f') = evalFnTree idMap f
    (ResFtn f False) -> (i, return ())
      where (i, _) = evalFnTree idMap f
    (ResOp o True) -> (i, outputStrLn $ show o')
      where (i, o') = evalOpTree idMap o
    (ResOp o False) -> (i, return ())
      where (i, o') = evalOpTree idMap o
    (ResNull) -> (idMap, return ())

mainloop :: IdMap -> InputT IO ()
mainloop idMap = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then "    " else ""
    case input of
        Nothing -> return ()
        Just s -> do case parseExpr (idMap, tokenize s) of
                         Nothing -> outputStrLn "parse error"
                         Just x -> do -- outputStrLn . show $ x
                                      let (idMap', out) = handleRes idMap x
                                      out
                                      -- outputStrLn . show $ idMap'
                                      mainloop idMap'

{-
        Just s -> do outputStrLn $ "tokens: " ++ (show . length $ s)
                     outputStrLn $ "expr: " ++ (show . parseExpr . tokenize $ s)
                     outputStrLn $ "der arr: " ++ (show . parseDerArr . tokenize $ s)
                     outputStrLn $ "train: " ++ (show . parseTrain . tokenize $ s)
                     outputStrLn $ "der fn: " ++ (show . parseDerFn . tokenize $ s)
                     outputStrLn $ "fn: " ++ (show . parseFn . tokenize $ s)
                     outputStrLn $ "arr: " ++ (show . parseArr . tokenize $ s)
                     outputStrLn $ "arr comp: " ++ (show . parseArrComp . tokenize $ s)
                     outputStrLn $ "all then max scalar " ++ (show . matchAllThenMax [parseScalar] . tokenize $ s)
                     outputStrLn $ "all " ++ (show . matchAll [parseScalar] . tokenize $ s)
                     outputStrLn $ "max " ++ (show . matchMax [parseScalar] . tokenize $ s)
                     outputStrLn $ "scalar " ++ (show . parseScalar . tokenize $ s)
                     mainloop
-}

main :: IO ()
main = runInputT settings (mainloop emptyIdMap)
    where settings = setComplete completeGlyph defaultSettings
