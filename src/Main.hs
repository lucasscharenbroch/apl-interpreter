import System.Console.Haskeline
import GlyphCompletion
import Lex (Token, tokenize)
import Parse
import Eval
import IdMap
import GrammarTree -- TODO remove

mainloop :: InputT IO ()
mainloop = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then "    " else ""
    case input of
        Nothing -> return ()
        Just s -> do case parseExpr (emptyIdMap, tokenize s) of
                         Nothing -> outputStrLn "parse error"
                         Just x -> do -- outputStrLn . show $ x
                                      case x of
                                          (ResAtn a) -> outputStrLn . show . evalArrTree $ a
                                          (ResFtn f) -> outputStrLn . show $ f
                                          (ResOp o) -> outputStrLn . show $ o
                                          (ResNull) -> return ()
                     mainloop

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
main = runInputT settings mainloop
    where settings = setComplete completeGlyph defaultSettings
