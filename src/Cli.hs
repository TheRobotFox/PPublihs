{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
-- | Commandline Interface

module Cli (cli) where
import Data.Data (Typeable)
import Data.List (find, intercalate)
import System.IO ( hFlush, stdout )
import Data.List.Split ( splitOn )
import Env (loadEnv, EnvironmentException, Config)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (lift)
import Control.Exception (Exception, catches, throwIO, Handler(Handler), SomeException)
import System.Directory (getCurrentDirectory)
import Module (getModules, runModule)
import Control.Monad (join)

data CLIException = NotImplemented deriving (Show, Typeable, Eq)
data ExitException = Exit deriving (Show)
instance Exception CLIException
instance Exception ExitException

type Cmd = ([String] -> Map String (Track String) -> StateT Config IO ())

commands :: [(String, String, Cmd)]
commands = [("help", "Print this page Commands", help),
            ("info", "Print info about Current Environment", info),
            ("run", "Run Module [modules...]", run),
            ("lsmod", "List available Modules", lift . join . fmap (putStrLn . show) . const getModules),
            ("exit", "Exit PPublihs", cmdError Exit),
            ("echo", "For testing", lift . putStrLn . show)]

cmdError :: Exception a => a -> Cmd
cmdError err = lift . throwIO . const . const err

help :: Cmd
help _ _ = lift . putStrLn . intercalate "\n" . map fmt $ commands
  where fmt (cmd, desc, _) = cmd ++ replicate (cmdLen - length cmd ) ' ' ++ " - " ++ desc
        cmdLen = maximum . map (length . \(x,_,_)->x) $ commands

info :: Cmd
info _ trks = lift . putStrLn . show $ trks

run :: Cmd
run ["all"] = run =<< lift getModules
run mods trks = lift $ mapM_ (runModule trks) mods

exec :: Env -> [String] -> IO ()
exec env (cmd:args) = case find (\(x,_,_)->x==cmd) commands of
    Just (_,_,fn) -> (flip runReaderT env $ fn args) `catches`
      [Handler (\(_ :: ExitException) -> throwIO Exit),
       Handler (\(e :: SomeException)-> putStrLn $ "An Error occured while executing command '"++cmd++"': " ++ show e)]
    Nothing -> putStrLn $ "Command not found!"
exec _ [] = mempty

cli :: StateT Config IO ()
cli = do
  env <- ask
  lift $ (do
    cd <- getCurrentDirectory
    putStr $ cd ++ " ~> "
    hFlush stdout
    inp <- getLine

    exec env . filter (/=[]) . splitOn " " $ inp)
    `catches` [Handler (\(_ :: ExitException) -> mempty),
               Handler (\(e :: EnvironmentException) -> putStrLn $ "Could not create Environment, please fix Issue: " ++ show e)]
  cli
