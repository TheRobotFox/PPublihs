{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
-- | Commandline Interface

module Cli where
import Control.Exception
    ( catches, throwIO, Handler(Handler), Exception, SomeException )
import Data.Data (Typeable)
import Data.List (find, intercalate)
import System.IO ( hFlush, stdout )
import Data.List.Split ( splitOn )
import State (getState, LocalState)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (lift)

data Exit = Exit deriving (Show, Typeable, Eq)
data NotImplemented = NotImplemented deriving (Show, Typeable, Eq)

instance Exception Exit
instance Exception NotImplemented


data Env = Env{state :: LocalState, render :: RenderSettings -> TrackName -> FilePath -> IO ()}

commands :: [(String, String, [String] -> ReaderT Env IO ())]
commands = [("help", "Print this page Commands", help),
            ("info", "Print info about Current Environment", info),
            ("run", "Run Module", cmdError NotImplemented),
            ("lsmod", "List available Modules", cmdError NotImplemented),
            ("exit", "Exit PPublihs", cmdError Exit),
            ("echo", "For testing", lift . putStrLn . show)]

cmdError :: Exception a => a -> [String] -> ReaderT Env IO ()
cmdError err = lift . throwIO . const err

help :: [String] -> ReaderT Env IO ()
help _ = lift . putStrLn . intercalate "\n" . map fmt $ commands
  where fmt (cmd, desc, _) = cmd ++ " - " ++ desc

info :: [String] -> ReaderT Env IO ()
info _ = do
  state <- ask
  lift . putStrLn . show $ state

exec :: Env -> [String] -> IO Bool
exec state (cmd:args) = case find (\(x,_,_)->x==cmd) commands of
    Just (cmd,_,fn) -> (flip runReaderT state $ fn args >> return True) `catches`
      [Handler (\(_ :: Exit) -> return False),
       Handler (\(e :: SomeException)->(putStrLn $ "An Error occured while executing command '"++cmd++"': " ++ show e) >> return True)]
    Nothing -> (putStrLn $ "Command not found!") >> return True

cli :: IO ()
cli = do
  state <- getState
  putStr "PPublihs ~> "
  hFlush stdout
  inp <- getLine
  shouldExit <- exec state . filter (/=[]) . splitOn " " $ inp
  if shouldExit then cli else return ()
