module Config (Config(..), defaultConfig) where

import           System.Process
import           Util (run)

data Config = Config {
  copyToClipboard  :: String -> IO ()
, openUrl          :: String -> IO ()
, generateUser     :: IO String
, generatePassword :: IO String
}

defaultConfig :: Config
defaultConfig = Config {
  copyToClipboard  = xclip
, openUrl          = xdgOpen
, generateUser     = pwgen Nothing
, generatePassword = pwgen (Just 20)
}

xclip :: String -> IO ()
-- vimperator, for some reason, needs -l 2, pentadactyl works with -l 1
-- xclip input = readProcess "xclip" ["-l", "2", "-quiet"] input >> return ()
xclip input = readProcess "xclip" ["-l", "1", "-quiet"] input >> return ()

xdgOpen :: String -> IO ()
xdgOpen url = run "xdg-open" [url]

pwgen :: Maybe Int -- ^ length of generated password
      -> IO String
pwgen mLength = fmap init $ readProcess "pwgen" args ""
  where
    args = "-s" : maybe [] (return . show) mLength
