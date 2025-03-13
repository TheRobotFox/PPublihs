{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
-- | Commandline Interface

module Cli where
import Control.Exception
import Data.Data (Typeable)
import Data.List (find, intercalate)
import System.IO

data Exit = Exit deriving (Show, Typeable, Eq)
data NotImplemented = NotImplemented deriving (Show, Typeable, Eq)

instance Exception Exit
instance Exception NotImplemented

help :: [(String, String)] -> IO ()
help = putStrLn . intercalate "\n" . map fmt
  where fmt (cmd, desc) = cmd ++ " - " ++ desc

commands :: [(String, String, IO ())]
commands = [
            ("help", "Print this page Commands", help . map (\(c,d,_)->(c,d)) $ commands),
            ("info", "Print info about Current Environment", throwIO NotImplemented >> return ()),
            ("run", "Run Module", throwIO NotImplemented >> return ()),
            ("lsmod", "List available Modules", throwIO NotImplemented >> return ()),
            ("exit", "Exit PPublihs", (throwIO Exit >> return ()))
           ]
-- info :: IO ()
-- info =


cli :: IO ()
cli = do
  putStr "PPublihs > "
  hFlush stdout
  inp <- getLine
  break <- case find (\(x,_,_)->x==inp) commands of
    Just (cmd,desc,fn) -> (fn >> return True) `catches`
      [Handler (\(e :: Exit) -> return False),
       Handler (\(e :: SomeException)->(putStrLn $ "An Error occured while executing command '"++cmd++"': " ++ show e) >> return True)]
    Nothing -> (putStrLn $ "Command not found!") >> return True

  if break then cli else return ()
