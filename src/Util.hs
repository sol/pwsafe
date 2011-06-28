module Util where

import           System.IO
import           System.Process
import           System.Exit
import           System.Directory
import           Control.Exception
import           Control.Monad
import           Control.DeepSeq
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
    ExitFailure n -> fail $ printf "%s %s exited with an error: %d" (show command) (show args) n

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile action = do
  tempdir <- getTemporaryDirectory
  (name, h) <- openTempFile tempdir ""
  finally (action name h) (hClose h >> removeFile name)

encrypt :: FilePath -> String -> IO ()
encrypt f s = do
  renameFile f $ f ++ ".old" -- backup file
  (Just inh, Nothing, Nothing, pid) <- createProcess $ (proc "gpg" ["--batch", "-e", "-a", "--default-recipient-self", "--output", f]) {std_in = CreatePipe}
  hPutStr inh s
  hClose inh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e

decrypt :: FilePath -> IO String
decrypt filename = do
  (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
  output <- hGetContents outh
  output `deepseq` hClose outh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e
  return output
