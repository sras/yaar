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
  ( HTML
  , PlainText
  , JSON
  , OctetStream
  , ReqBody
  , RequestHeader
  , ResponseHeader
  , addHeader
  , NoContent(..)
  )
where

import Yaar.Core
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack, Text)
import Data.Aeson
import Network.Wai
  ( requestBody
  , requestHeaders
  , mapResponseHeaders
  , responseLBS
  , queryString
  )
import Network.HTTP.Types.Status (Status, status400, status200)
import Network.HTTP.Types (HeaderName, Header)
import GHC.TypeLits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.String
import Data.Proxy

data OctetStream

data NoContent = NoContent

instance ContentType NoContent where
  getContentType _ = Nothing

instance Encodable '[] NoContent where
  encode _ _ = ""

-- to add support for input coming in request body
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

-- For output in json
instance ContentType JSON where
  getContentType _ = Just "application/json"

instance (ToJSON a) => Encodable JSON a where
  encode v _ =  LB.toStrict $ Data.Aeson.encode $ toJSON v

-- to implement input via header

data RequestHeader (s :: Symbol) a = RequestHeader a

type instance UrlToRequestDerivable (RequestHeader s a) = RequestHeader s a

instance (Read a, KnownSymbol s) => RequestDerivable (RequestHeader s a) where
  extract request = return $ extractHeaderValue (requestHeaders request) (symbolVal (Proxy :: Proxy s))
    where
      extractHeaderValue :: (Read a) => [Header] -> String -> Either Status (RequestHeader s a)
      extractHeaderValue headers headerName =
        case lookup (fromString headerName) headers of
          Just x -> Right $ RequestHeader $ read $ unpack $ decodeUtf8 $ x
          Nothing -> Left $ status400

instance Convertable (RequestHeader s a) a where
  convert (RequestHeader a) = a

-- to implement support for query parameters

data QueryParam s a = QueryParam a

type instance UrlToRequestDerivable (QueryParam s a) = QueryParam s a

instance (KnownSymbol s, Read a) => RequestDerivable (QueryParam s (Maybe a)) where
  extract r =
    case lookup (encodeUtf8 $ pack $ symbolVal (Proxy :: Proxy s)) $ queryString r of
      Just (Just a) -> return $ Right $ QueryParam $ read $ unpack $ decodeUtf8 a
      Just Nothing -> return $ Right $ QueryParam $ Nothing
      Nothing -> return $ Right $ QueryParam Nothing

--  to implement output Headers
data ResponseHeader (s :: [Symbol]) a = ResponseHeader [(HeaderName, ByteString)] a

type family WrappedInHeader a where
  WrappedInHeader (ResponseHeader s a) = a
  WrappedInHeader a = a

type family AHParent a where
  AHParent (ResponseHeader s a) = s
  AHParent a = '[]

class HeaderAttachable a where
  addHeader :: (KnownSymbol s1) => (Proxy s1) -> Text -> a -> ResponseHeader (s1:AHParent a) (WrappedInHeader a)

instance  {-# OVERLAPPABLE #-} (AHParent a ~ '[], WrappedInHeader a ~ a) => HeaderAttachable a where
  addHeader (p :: Proxy s) v a =
    let
      headerName = symbolVal (Proxy :: Proxy s)
      in (ResponseHeader [(fromString headerName, encodeUtf8 v)]  a) :: ResponseHeader (s:'[]) a

instance HeaderAttachable (ResponseHeader s2 a) where
  addHeader (p :: Proxy s1) v (ResponseHeader s a) =
    let
      headerName = symbolVal (Proxy :: Proxy s1)
    in (ResponseHeader ((fromString headerName, encodeUtf8 v):s) a) :: ResponseHeader (s1:s2) a

instance (ToResponse format value) => ToResponse format (ResponseHeader headerNames value) where
  toResponse (ResponseHeader headers v) p = mapResponseHeaders (\x -> headers ++ x) $ toResponse v p
--
data HTML

data PlainText

instance Encodable PlainText Text where
  encode a _ = encodeUtf8 $ a

instance Encodable HTML Text where
  encode a _ = encodeUtf8 $ a

instance Encodable PlainText String where
  encode a _ = encodeUtf8 $ pack $ a

instance Encodable HTML String where
  encode a _ = encodeUtf8 $ pack $ a

instance ContentType PlainText where
  getContentType _ = Just "text/plain"

instance ContentType HTML where
  getContentType _ = Just "text/html"
