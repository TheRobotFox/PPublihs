module Main (main) where

import Cli
import Env
import Module (generateDefaultModules)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

main :: IO ()
main = do
  cfg <- getSettings
  generateDefaultModules
  cli cfg
