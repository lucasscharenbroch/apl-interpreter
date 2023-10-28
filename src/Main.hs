import System.Console.Haskeline
import GlyphCompletion
import Lex (Token, tokenize)

mainloop :: InputT IO ()
mainloop = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then "    " else ""
    case input of
        Nothing -> return ()
        Just s -> do outputStrLn . show . tokenize $ s
                     mainloop
main :: IO ()
main = runInputT settings mainloop
    where settings = setComplete completeGlyph defaultSettings
