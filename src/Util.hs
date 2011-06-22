module Util where

import Network.URI


nameFromUrl :: String -> String
nameFromUrl url = case parseURI url of
  Nothing  -> error $ url ++ " is no valid url!"
  Just uri -> case uriAuthority uri of
    Nothing -> "none"
    Just x -> host
      where host = uriRegName x
