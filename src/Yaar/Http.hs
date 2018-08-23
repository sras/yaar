{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
{-# Language TypeOperators #-}
{-# Language ScopedTypeVariables #-}

module Yaar.Http
--   ( HTML
--   , PlainText
--   , JSON
--   , OctetStream
--   , ReqBody
--   , Header
--   )
where

import Yaar.Core
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack, unpack, Text)
import Data.Aeson
import Network.Wai (requestBody)
import Network.HTTP.Types.Status (status400)
import GHC.TypeLits
import Data.Proxy

data OctetStream


-- Code to add support for input coming in request body
data JSON

data ReqBody s a = ReqBody a

type instance UrlToRequestDerivable (ReqBody s a) = ReqBody s a

instance (FromJSON a) => RequestDerivable (ReqBody JSON a) where
  extract req = do
    body <- requestBody req
    return $ case eitherDecodeStrict body of
      Right a -> Right $ ReqBody a
      Left a -> Left $ status400

instance Convertable (ReqBody s a) a where
  convert (ReqBody a) = a

-- Code to implement input via header

data Header (s :: Symbol) a = Header a

type instance UrlToRequestDerivable (Header s a) = Header s a

instance (Read a, KnownSymbol s) => RequestDerivable (Header s a) where
  extract request = return $ Right $ Header (read "")

instance Convertable (Header s a) a where
  convert (Header a) = a
--
-- Code to implement output Headers

data ResponseHeader (s :: [Symbol]) a = ResponseHeader a

type family WrappedInHeader a where
  WrappedInHeader (ResponseHeader s a) = a
  WrappedInHeader a = a

class HeaderAttachable a (s :: [Symbol]) where
  addHeader :: (KnownSymbol s1) => (Proxy s1) -> a -> ResponseHeader (s1:s) (WrappedInHeader a)

instance (WrappedInHeader a ~ a) => HeaderAttachable a '[] where
  addHeader (p :: Proxy s) a = ResponseHeader a :: ResponseHeader '[s] a
-- 
instance HeaderAttachable (ResponseHeader s2 a) s2 where
  addHeader (p :: Proxy s1) (ResponseHeader a) = ResponseHeader a :: ResponseHeader (s1:s2) a
-- 
--
data HTML

data PlainText

instance (Show a) => Encodable PlainText a where
  encode a _ = encodeUtf8 $ pack $ show a

instance (Show a) => Encodable HTML a where
  encode a _ = encodeUtf8 $ pack $ show a

instance ContentType PlainText where
  toContentType _ = "text/plain; charset=utf-8"

instance ContentType HTML where
  toContentType _ = "text/html"
