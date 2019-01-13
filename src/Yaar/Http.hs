{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}
{-# Language TypeOperators #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yaar.Http
  ( HTML
  , PlainText
  , JSON
  , OctetStream
  , RequestBody
  , RequestHeader
  , ResponseHeader
  , QueryParam
  , addHeader
  , NoContent(..)
  )
where

import Yaar.Core
import Yaar.Autodoc
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text as T (concat, pack, Text)
import Data.Aeson
import Network.Wai
  ( lazyRequestBody
  , requestHeaders
  , mapResponseHeaders
  , responseLBS
  , queryString
  )
import Network.HTTP.Types.Status (Status(..), status400, status200)
import Network.HTTP.Types (HeaderName, Header)
import GHC.TypeLits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.String
import Data.Proxy
import Web.HttpApiData

data OctetStream

data NoContent = NoContent

instance ContentType NoContent where
  getContentType _ = Nothing

instance Encodable NoContent NoContent where
  encode _ _ = ""

instance Handler (ResponseFormat NoContent (YaarHandler a)) where
  execute _ (ResponseFormat h) = do
    _ <- h
    return $ responseLBS status200 [] ""

-- to add support for input coming in request body
data JSON

data RequestBody s a = RequestBody a

instance {-# OVERLAPPING #-} (ToYaarSchema a) => RouteInfoSegment (RequestBody '[] a) where
  addRouteInfo a = (\x -> x {routeRequestBody = Just $ toYaarSchema (Proxy :: Proxy a)})

instance {-# OVERLAPPING #-} (ContentType format, ToYaarSchema a, RouteInfoSegment (RequestBody xs a)) => RouteInfoSegment (RequestBody (format:xs) a) where
  addRouteInfo a =
    let
      ri = addRouteInfo (Proxy :: Proxy (RequestBody xs a))
    in 
    case getContentType (Proxy :: Proxy format) of
        Just f -> 
          (\x -> let rx = ri x in rx { routeRequestBodyFormat = f:routeRequestBodyFormat rx})
        Nothing -> ri

instance (KnownSymbol s, ToYaarSchema a) => RouteInfoSegment (RequestHeader s a) where
  addRouteInfo a = let
    addHeader :: [(String, YaarSchema)] -> [(String, YaarSchema)]
    addHeader i = (symbolVal (Proxy :: Proxy s), toYaarSchema (Proxy :: Proxy a)):i
    in (\x -> x { routeRequestHeaders = addHeader $ routeRequestHeaders x }) 

type instance RequestDerivableToHandlerArg (RequestBody s a) = a
type instance RequestDerivableToHandlerArg (RequestHeader s a) = a

type instance UrlToRequestDerivable (RequestBody s a) = RequestBody s a

instance (RequestDerivable (RequestBody f a), RequestDerivable (RequestBody t a)) => RequestDerivable (RequestBody (f:t) a) where
  extract req = do
    a :: Either Status (RequestBody f a) <- extract req
    case a of
      Right (RequestBody b) -> pure $ Right (RequestBody b)
      Left _ -> do
        b :: Either Status (RequestBody t a) <- extract req
        case b of
          Right (RequestBody c) -> pure $ Right (RequestBody c)
          Left x -> pure $ Left x

instance RequestDerivable (RequestBody '[] a) where
  extract _ = pure $ Left status400 { statusMessage = encodeUtf8 $ "Unsupported format in request body"}

instance (FromJSON a) => RequestDerivable (RequestBody JSON a) where
  extract req = do
    body <- lazyRequestBody req
    return $ case eitherDecode body of
      Right a -> Right $ RequestBody a
      Left err -> Left $ status400 { statusMessage = encodeUtf8 $ pack $ "Decoding error" ++ err }

instance Convertable (RequestBody s a) a where
  convert (RequestBody a) = a

-- For output in json
instance ContentType JSON where
  getContentType _ = Just "application/json"

instance (ToJSON a) => Encodable JSON a where
  encode v _ =  LB.toStrict $ Data.Aeson.encode $ toJSON v

-- to implement input via header
data RequestHeader (s :: Symbol) a = RequestHeader a

type instance UrlToRequestDerivable (RequestHeader s a) = RequestHeader s a

instance (FromHttpApiData a, KnownSymbol s) => RequestDerivable (RequestHeader s a) where
  extract request = return $ extractHeaderValue (requestHeaders request) (symbolVal (Proxy :: Proxy s))
    where
      extractHeaderValue :: [Header] -> String -> Either Status (RequestHeader s a)
      extractHeaderValue headers headerName =
        case lookup (fromString headerName) headers of
          Just x -> case parseHeader x of
            Right x_ -> Right $ RequestHeader $ x_
            Left err -> Left $ status400 { statusMessage = encodeUtf8 $ T.concat ["Decoding error: ", err] }
          Nothing -> Left $ status400 { statusMessage = encodeUtf8 $ pack $ "Header expected not found: " ++ headerName }

instance Convertable (RequestHeader s a) a where
  convert (RequestHeader a) = a

-- to implement support for query parameters

data QueryParam s a = QueryParam a

type instance UrlToRequestDerivable (QueryParam s a) = QueryParam s (Maybe a)

type instance RequestDerivableToHandlerArg (QueryParam s (Maybe a)) = Maybe a

instance (KnownSymbol s, FromHttpApiData a) => RequestDerivable (QueryParam s (Maybe a)) where
  extract r =
    case lookup (encodeUtf8 $ pack $ symbolVal (Proxy :: Proxy s)) $ queryString r of
      Just (Just a) -> case parseHeader a of
        Right a_ -> pure $ Right $ QueryParam $ Just a_
        Left err -> pure $ Left $ status400 { statusMessage = encodeUtf8 $ T.concat [ "Decoding failed for query item :", err] }
      Just Nothing -> return $ Right $ QueryParam $ Nothing
      Nothing -> return $ Right $ QueryParam Nothing

instance Convertable (QueryParam s a) a where
  convert (QueryParam a) = a

instance (KnownSymbol a, ToYaarSchema b) => RouteInfoSegment (QueryParam a b) where
  addRouteInfo a = let
    addQuery :: [(String, YaarSchema)] -> [(String, YaarSchema)]
    addQuery in_ = ( (symbolVal (Proxy :: Proxy a)), toYaarSchema (Proxy :: Proxy b) ):in_
    in (\x ->
         x { routeQuery = addQuery (routeQuery x) })

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
  addHeader (_ :: Proxy s) v a =
    let
      headerName = symbolVal (Proxy :: Proxy s)
      in (ResponseHeader [(fromString headerName, encodeUtf8 v)]  a) :: ResponseHeader (s:'[]) a

instance HeaderAttachable (ResponseHeader s2 a) where
  addHeader (_ :: Proxy s1) v (ResponseHeader s a) =
    let
      headerName = symbolVal (Proxy :: Proxy s1)
    in (ResponseHeader ((fromString headerName, encodeUtf8 v):s) a) :: ResponseHeader (s1:s2) a

instance (ToResponse format value) => ToResponse format (ResponseHeader headerNames value) where
  toResponse (ResponseHeader headers v) p = mapResponseHeaders (\x -> headers ++ x) $ toResponse v p
--
data HTML

data PlainText

instance Encodable OctetStream ByteString where
  encode a _ = a

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

instance ContentType OctetStream where
  getContentType _ = Nothing
