{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
-- | Commandline Interface

module Cli (cli) where
import Data.Data (Typeable)
import Data.List (find, intercalate)
import System.IO ( hFlush, stdout )
import Data.List.Split ( splitOn )
import Env (loadEnv, Env(..), EnvironmentException)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (lift)
import Settings (Settings)
import Control.Exception (Exception, catches, throwIO, Handler(Handler), SomeException)
import System.Directory (getCurrentDirectory)
import Module (getModules, runModule)
import Control.Monad (join)

data CLIException = NotImplemented deriving (Show, Typeable, Eq)
data ExitException = Exit deriving (Show)
instance Exception CLIException
instance Exception ExitException


commands :: [(String, String, [String] -> ReaderT Env IO ())]
commands = [("help", "Print this page Commands", help),
            ("info", "Print info about Current Environment", info),
            ("run", "Run Module [modules...]", run),
            ("lsmod", "List available Modules", lift . join . fmap (putStrLn . show) . const getModules),
            ("exit", "Exit PPublihs", cmdError Exit),
            ("echo", "For testing", lift . putStrLn . show)]

cmdError :: Exception a => a -> [String] -> ReaderT Env IO ()
cmdError err = lift . throwIO . const err

help :: [String] -> ReaderT Env IO ()
help _ = lift . putStrLn . intercalate "\n" . map fmt $ commands
  where fmt (cmd, desc, _) = cmd ++ replicate (cmdLen - length cmd ) ' ' ++ " - " ++ desc
        cmdLen = maximum . map (length . \(x,_,_)->x) $ commands

info :: [String] -> ReaderT Env IO ()
info _ = do
  env <- ask
  lift . putStrLn . show . state $ env

run :: [String] -> ReaderT Env IO ()
run ["all"] = run =<< lift getModules
run mods = do
  env <- ask
  lift $ mapM_ (runModule env) mods

exec :: Env -> [String] -> IO ()
exec env (cmd:args) = case find (\(x,_,_)->x==cmd) commands of
    Just (_,_,fn) -> (flip runReaderT env $ fn args) `catches`
      [Handler (\(_ :: ExitException) -> throwIO Exit),
       Handler (\(e :: SomeException)-> putStrLn $ "An Error occured while executing command '"++cmd++"': " ++ show e)]
    Nothing -> putStrLn $ "Command not found!"
exec _ [] = mempty

cli :: Settings -> IO ()
cli cfg = (do
  cd <- getCurrentDirectory
  putStr $ cd ++ " ~> "
  hFlush stdout
  inp <- getLine
  env <- (loadEnv cfg)

  exec env . filter (/=[]) . splitOn " " $ inp
  cli cfg)
    `catches` [Handler (\(_ :: ExitException) -> mempty),
               Handler (\(e :: EnvironmentException) -> putStrLn $ "Could not create Environment, please fix Issue: " ++ show e)]
