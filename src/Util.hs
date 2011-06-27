module Util where

import Network.URI


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
