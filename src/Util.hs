module Util where

import           Data.List
import qualified Data.Char as Char
import           System.IO
import           System.Process
import           System.Exit
import           System.Directory
import           Control.Exception
import           Network.URI
import           Text.Printf

nameFromUrl :: String -> String
nameFromUrl url = case parseURI url of
  Nothing  -> error $ url ++ " is not a valid URL!"
  Just uri -> case uriAuthority uri of
    Nothing -> "none"
    Just x -> host
      where host = uriRegName x

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p a a' = do
  b <- p
  if b then a else a'

run :: String -> [String] -> IO ()
run command args = do
  e <- rawSystem command args
  case e of
    ExitSuccess   -> return ()
    ExitFailure n -> error $ printf "%s %s exited with an error: %d" (show command) (show args) n

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile action = do
  tempdir <- getTemporaryDirectory
  (name, h) <- openTempFile tempdir ""
  finally (action name h) (hClose h >> removeFile name)

data MatchResult = None | Match String | Ambiguous [String]
  deriving (Eq, Show)

match :: String -> [String] -> MatchResult
match s l = case filter (isInfixOf (toLower s) . toLower) l of
  []  -> None
  [x] -> Match x
  xs   -> if s `elem` xs then Match s else Ambiguous xs
  where
    toLower = map Char.toLower
