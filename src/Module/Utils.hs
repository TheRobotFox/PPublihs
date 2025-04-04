-- |

module Module.Utils where

getModules :: IO [FilePath]
getModules = do cfgDir <- getXdgDirectory XdgConfig appName
                listDirectory $ combine cfgDir "modules"

defaultModules :: [(String, RenderSettings)]
defaultModules = [("flac", (Flac, [])),
                  ("mp3", (Mp3, [])),
                  ("amuse", (Wav, ["-a 44100"])),
                  ("full", (Mp3 [])),
                  ("video", MergedRender [File Video] [] "mp4")]

generateDefaultModules :: IO ()
generateDefaultModules = do
  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  createDirectoryIfMissing True modDir
  let write (mod', cfg) = BSL.writeFile (combine modDir mod') . encode $ cfg

  present <- getModules
  mapM_ write $ Data.List.filter (not . (`elem` present) . fst) defaultModules
