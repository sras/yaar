{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}

module Yaar.Http
  ( HTML
  , PlainText
  , JSON
  , OctetStream
  , ReqBody
  )
where

import Yaar.Core
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack, unpack, Text)
import Data.Aeson
import Network.Wai (requestBody)
import Network.HTTP.Types.Status (status400)

data HTML
data PlainText
data JSON
data OctetStream

data ReqBody s a = ReqBody a

-- Code to add support for input coming in request body
type instance UrlToRequestDerivable (ReqBody s a) = ReqBody s a

instance (FromJSON a) => RequestDerivable (ReqBody JSON a) where
  extract req = do
    body <- requestBody req
    return $ case eitherDecodeStrict body of
      Right a -> Right $ ReqBody a
      Left a -> Left $ status400

instance Convertable (ReqBody s a) a where
  convert (ReqBody a) = a
--
instance (Show a) => Encodable PlainText a where
  encode a _ = encodeUtf8 $ pack $ show a

instance (Show a) => Encodable HTML a where
  encode a _ = encodeUtf8 $ pack $ show a

instance ContentType PlainText where
  toContentType _ = "text/plain; charset=utf-8"

instance ContentType HTML where
  toContentType _ = "text/html"
