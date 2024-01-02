module Exceptions where
import Control.Exception
import System.IO

data ExecErr = RankError String
             | IndexError String
             | DomainError String
             | LengthError String
             | WipError String -- TODO remove

instance Show ExecErr where
    show (RankError s) = "Rank Error: " ++ s
    show (IndexError s) = "Index Error: " ++ s
    show (DomainError s) = "Domain Error: " ++ s
    show (LengthError s) = "Length Error: " ++ s
    show (WipError s) = "Not Yet Implemented: " ++ s -- TODO remove

instance Exception ExecErr

handleExecErr :: ExecErr -> IO (Maybe a)
handleExecErr e = hPutStrLn stderr (show e) >> return Nothing

catchExecErr :: IO a -> IO (Maybe a)
catchExecErr ioa = catch (Just <$> ioa) handleExecErr
