{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
-- | Commandline Interface

module Cli (cli) where
import Data.Data (Typeable)
import Data.List (find, intercalate)
import System.IO ( hFlush, stdout )
import Data.List.Split ( splitOn )
import Env (EnvironmentException, Config, loadTracks)
import Control.Monad.Trans.Class (lift)
import Control.Exception (Exception, catches, throwIO, Handler(Handler), IOException)
import System.Directory (getCurrentDirectory)
import Module (getModules, runModule)
import Control.Monad (join, forever)
import Control.Monad.Trans.State
import Track (Track, getChecksum)
import Data.Map (Map)
import Render (render)

data CLIException = NotImplemented deriving (Show, Typeable, Eq)
data ExitException = Exit deriving (Show)
instance Exception CLIException
instance Exception ExitException

type Cmd = (Map String (Track String) -> [String] -> StateT Config IO ())

commands :: [(String, String, Cmd)]
commands = [("help", "Print this page Commands", help),
            ("info", "Print info about Current Environment", info),
            ("run", "Run Module [modules...]", run),
            ("lsmod", "List available Modules", \_ _ -> lift . join . fmap (putStrLn . show) $ getModules),
            ("exit", "Exit PPublihs", cmdError Exit),
            ("echo", "For testing", const (lift . putStrLn . show))]

cmdError :: Exception a => a -> Cmd
cmdError err = const (lift . throwIO . const err)

help :: Cmd
help _ _ = lift . putStrLn . intercalate "\n" . map fmt $ commands
  where fmt (cmd, desc, _) = cmd ++ replicate (cmdLen - length cmd ) ' ' ++ " - " ++ desc
        cmdLen = maximum . map (length . \(x,_,_)->x) $ commands

info :: Cmd
info _ trks = lift . putStrLn . show $ trks

run :: Cmd
run trks ["all"] = run trks =<< lift getModules
run trks mods = do
  cks <- lift . mapM getChecksum $ trks
  lift $ mapM_ (runModule cks <*> (render trks)) mods

exec :: Cmd
exec env (cmd:args) = case find (\(x,_,_)->x==cmd) commands of
    Just (_,_,fn) -> (fn env args)
    Nothing -> lift . putStrLn $ "Command not found!"
exec _ [] = return ()

catchesState :: StateT Config IO a -> [Handler a] -> StateT Config IO a
catchesState (StateT f) handlers = StateT $ \s0 -> (f s0) `catches` map (fmap (flip (,) s0)) handlers

cli :: Config -> IO ()
cli cfg = fmap fst . flip runStateT cfg . forever $ do

  tracks <- lift . loadTracks $ cfg
  inp <- lift $ do
    cd <- getCurrentDirectory
    putStr $ cd ++ " ~> "
    hFlush stdout
    getLine

  (exec tracks . filter (/=[]) . splitOn " " $ inp)
    `catchesState`
    [Handler (\(e :: IOException) -> putStrLn $ "An Error occured while executing command '"++inp++"': " ++ show e)]

 `catchesState`
    [Handler (\(_ :: ExitException) -> mempty),
     Handler (\(e :: EnvironmentException) -> putStrLn $ "Could not create Environment, please fix Issue: " ++ show e)]
