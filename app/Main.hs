module Main (main) where

import Cli
import Settings
import Module (generateDefaultModules)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

settingsDialog =

main :: IO ()
main = do
  cfg <- getSettings
  generateDefaultModules
  cli cfg
