module Main (main) where

import Cli
import Settings (getSettings, askAll)
import Module (generateDefaultModules)


main :: IO ()
main = do
  cfg <- getSettings askAll
  generateDefaultModules
  cli cfg
