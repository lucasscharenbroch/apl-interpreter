import System.Console.Haskeline
import GlyphCompletion
import Lex (Token, tokenize)
import Parse
import Eval
import GrammarTree -- TODO remove

mainloop :: InputT IO ()
mainloop = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then "    " else ""
    case input of
        Nothing -> return ()
        Just s -> do case parseStatement . tokenize $ s of
                         Nothing -> outputStrLn "parse error"
                         Just xs -> outputStrLn . show . head $ xs
                     mainloop
{-
        Just s -> do case parseStatement . tokenize $ s of
                         Nothing -> outputStrLn "parse error"
                         Just xs -> case head xs of
                                        (ResArr a) -> outputStrLn . show . evalArrTree $ a
                     mainloop
-}


{-
        Just s -> do outputStrLn $ "tokens: " ++ (show . length $ s)
                     outputStrLn $ "statement: " ++ (show . parseStatement . tokenize $ s)
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

-}
                     mainloop
main :: IO ()
main = runInputT settings mainloop
    where settings = setComplete completeGlyph defaultSettings
