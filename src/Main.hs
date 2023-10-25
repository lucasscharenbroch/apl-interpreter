import System.Console.Haskeline

mainloop :: InputT IO ()
mainloop = do
    isInterractive <- haveTerminalUI
    input <- getInputLine $ if isInterractive then "    " else ""
    case input of
        Nothing -> return ()
        Just s -> do outputStrLn $ "input was: " ++ s
                     mainloop

main :: IO ()
main = runInputT defaultSettings mainloop
