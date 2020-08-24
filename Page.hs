module Page where

import Data.Proxy

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Req
import Text.URI

get :: Text -> IO (Maybe BsResponse)
get url = runReq httpCfg $ do
  case useURI =<< (mkURI url) of
    Nothing  -> pure Nothing
    Just uri -> Just <$> either getBS getBS uri
  where
    getBS (uri, schema) = req GET uri NoReqBody (Proxy :: Proxy BsResponse) schema
    httpCfg = defaultHttpConfig
