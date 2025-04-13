module Main (main) where

import Cli (cli, Env (Env))
import Env (loadTracks, getSettings)
import Module (generateDefaultModules)
import Control.Monad.Trans.State (StateT(runStateT))

main :: IO ()
main = do
  generateDefaultModules
  cfg <- getSettings
  tracks <- loadTracks $ cfg
  -- load state file
  newState <- fmap snd . runStateT cli $ Env tracks cfg
  -- store state file
  return ()
