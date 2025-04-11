{-# LANGUAGE ScopedTypeVariables #-}
-- | Commandline Interface

module Cli (cli) where
import Data.Data (Typeable)
import Data.List (find, intercalate, sortOn, transpose)
import System.IO ( hFlush, stdout )
import Data.List.Split ( splitOn )
import Env (EnvironmentException, Config, loadTracks, EnvField (..), appName)
import Control.Monad.Trans.Class (lift)
import Control.Exception (Exception, catches, throwIO, Handler(Handler), IOException)
import System.Directory (getCurrentDirectory)
import Control.Monad (join, forever)
import Control.Monad.Trans.State
import Track (Track (..), getAudioLength, Metadata (..), Attr (..), sortTracks)
import Data.Map (Map, toList, (!))
import Render (render)
import Numeric (showFFloat)
import Module (getModules, run, ModuleState (..))
import Persistate (runPersistate)
import Module.Env (ModuleConfig(None))
import System.FilePath (combine)

data CLIException = NotImplemented deriving (Show, Typeable, Eq)
data ExitException = Exit deriving (Show)
instance Exception CLIException
instance Exception ExitException

type Cmd = (Map String Track -> [String] -> StateT Config IO ())

commands :: [(String, String, Cmd)]
commands = [("help", "Print this page Commands", help),
            ("info", "Print info about Current Environment", info),
            ("sync", "Sync Module [modules ...]", sync),
            ("lsmod", "List available Modules", \_ _ -> lift . join . fmap (putStrLn . unlines) $ getModules),
            ("exit", "Exit PPublihs", cmdError Exit),
            ("echo", "For testing", const (lift . putStrLn . show))]

cmdError :: Exception a => a -> Cmd
cmdError err = const (lift . throwIO . const err)

help :: Cmd
help _ _ = lift . putStrLn . intercalate "\n" . map fmt $ commands
  where fmt (cmd, desc, _) = cmd ++ replicate (cmdLen - length cmd ) ' ' ++ " - " ++ desc
        cmdLen = maximum . map (length . \(x,_,_)->x) $ commands

fmtTable :: [[String]] -> String
fmtTable = unlines . map concat . transpose . map (flip padCol <*> (+1) . foldr max 0 . map length)
  where padCol p = map ((++) <*> (flip replicate ' ' . (-) p . length))

info :: Cmd
info trkList _ =do
  cfg <- get
  lift . putStrLn $ "Album: " ++ cfg!(MD . Attr $ Album)

  lift . putStrLn $ "--- Tracks ---"

  tracks <- lift . mapM (uncurry fmtTrack) . sortTracks $ trkList

  lift . putStrLn . fmtTable . transpose . (:) ["Nr", "Track", "", "Length"] $ tracks

  lift . putStrLn $ "--- Configuration ---"
  lift . putStrLn . fmtTable . transpose . (:) ["Option", "Value"] . map (liftA2 (:) (show . fst) (return . snd)) . toList $ cfg
  return ()

  where fmtTrack name track = do
          len <- getAudioLength . path $ track
          return [(metadata track)!(Attr Nr) ++ ".", name, ":", showFFloat (Just 2) len "s"]

sync :: Cmd
sync trks ["all"] = sync trks =<< lift getModules
sync trks mods = do
  lift $ mapM_ runMod mods
  where runMod mod = runPersistate (combine "cache" mod) (ModuleState mempty None mempty) $ run mod trks

exec :: Cmd
exec env (cmd:args) = case find (\(x,_,_)->x==cmd) commands of
    Just (_,_,fn) -> (fn env args)
    Nothing -> lift . putStrLn $ "Command not found!"
exec _ [] = return ()

catchesState :: StateT Config IO a -> [Handler a] -> StateT Config IO a
catchesState (StateT f) handlers = StateT $ \s0 -> (f s0) `catches` map (fmap (flip (,) s0)) handlers

cli :: Config -> IO ()
cli cfg = (fmap fst . flip runStateT cfg . forever $ do

  inp <- lift $ do
    cd <- getCurrentDirectory
    putStr $ cd ++ " ~> "
    hFlush stdout
    getLine

  tracks <- lift . loadTracks $ cfg
  (exec tracks . filter (/=[]) . splitOn " " $ inp)
    `catchesState`
    [Handler (\(e :: IOException) -> putStrLn $ "An Error occured while executing command '"++inp++"': " ++ show e)]
          )
 `catches`
    [Handler (\(_ :: ExitException) -> return ()),
     Handler (\(e :: EnvironmentException) -> putStrLn ("Could not create Environment, please fix Issue: " ++ show e))]
