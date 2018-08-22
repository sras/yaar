{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language RankNTypes #-}
{-# Language KindSignatures #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language UndecidableInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

module Yaar.Core 
  ( (:>)
  , (<|>)
  , Endpoint(..)
  , ToEndpoint(..)
  , type (<|>)
  , UrlParam
  , GET
  , HEAD
  , POST
  , PUT
  , DELETE
  , CONNECT
  , OPTIONS
  , PATCH
  , serve
  , Convertable(..)
  , ContentType(..)
  , Encodable(..)
  , UrlToRequestDerivable
  , RequestDerivable(..)
  ) 
where

import GHC.TypeLits as TL
import Data.Proxy
import Data.List
import Data.Maybe

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Network.Wai
  ( Application
  , responseLBS
  , Request
  , Response
  , ResponseReceived
  , requestMethod
  , pathInfo
  )
import Network.HTTP.Types (Status, StdMethod)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200, status400, status415, status404)
import Yaar.Routing

-- some boiler plate to convert type level lists to value level list
--
class ManySymbols a where
  toSymbolList :: Proxy a -> [Text]

instance ManySymbols '[] where
  toSymbolList _ = []

instance (ManySymbols a, KnownSymbol x) => ManySymbols (x:a) where
  toSymbolList _ = (pack $ symbolVal (Proxy :: Proxy x)):(toSymbolList (Proxy :: Proxy a))

class ManySymbolLists a where
  toSymbolLists :: Proxy a -> [[Text]]

instance ManySymbolLists '[] where
  toSymbolLists _ = []

instance (ManySymbolLists a, ManySymbols x) => ManySymbolLists (x:a) where
  toSymbolLists _ = toSymbolList (Proxy :: Proxy x) : (toSymbolLists (Proxy :: Proxy a))

-- Possible handler types
data GET format a
data HEAD format a
data POST format a
data PUT format a
data DELETE format a
data CONNECT format a
data OPTIONS format a
data PATCH format a
data CUSTOM (s :: Symbol) format a

data ResponseFormat format s = ResponseFormat s

data UrlParam (segment::Symbol) a = UrlParam a

data a :> b

infixr 9 :>

type Endpoint = IO

type family IsEqual a b where
  IsEqual a a = a
  IsEqual a b = TypeError
    ( TL.Text "Endpoint handlers should be all of one type, but found at leaset two of them, '"
      :<>: TL.ShowType a
      :<>: (TL.Text "'")
      :<>: (TL.Text " and '")
      :<>: TL.ShowType b
      :<>: (TL.Text "'"))

type family ExtractTC a :: * -> * where
  ExtractTC (a <|> b) = IsEqual (ExtractTC a) (ExtractTC b)
  ExtractTC (a -> b) = ExtractTC b
  ExtractTC (m a) = m
  ExtractTC b = TypeError
    (TL.Text "Endpoint handlers should return a parametrized type of form 'm a', but found '"
     :<>: TL.ShowType b
     :<>: (TL.Text "'"))

type family UrlToRequestDerivable a

type instance UrlToRequestDerivable  (UrlParam s a) = (UrlParam s a)

type family ExtractHandler (a :: *)  where
  ExtractHandler ((a :: Symbol) :> b) = (ExtractHandler b)
  ExtractHandler (a :> b) = (UrlToRequestDerivable a) -> (ExtractHandler b)
  ExtractHandler (b s a) = (ResponseFormat s (Endpoint a))

type family ExtractUrl a :: [Symbol] where
  ExtractUrl ((UrlParam s a) :> b) = s : "::param::": ExtractUrl b
  ExtractUrl ((a :: Symbol) :> b) = a : ExtractUrl b
  ExtractUrl (a :> b) = ExtractUrl b
  ExtractUrl (GET _ a) = '["GET"]
  ExtractUrl (HEAD _ a) = '["HEAD"]
  ExtractUrl (POST _ a) = '["POST"]
  ExtractUrl (PUT _ a) = '["PUT"]
  ExtractUrl (DELETE _ a) = '["DELETE"]
  ExtractUrl (CONNECT _ a) = '["CONNECT"]
  ExtractUrl (OPTIONS _ a) = '["OPTIONS"]
  ExtractUrl (PATCH _ a) = '["PATCH"]
  ExtractUrl (CUSTOM s _ a) = '[s]

type family ReturnType a where
  ReturnType (a -> b) = ReturnType b
  ReturnType a = a

type family ExtractUrlList a :: [[Symbol]] where
  ExtractUrlList (a <|> b) = (ExtractUrl a):(ExtractUrlList b)
  ExtractUrlList a = '[ExtractUrl a]

class RequestDerivable a where
  extract :: Request -> IO (Either Status a)

lookupUrlParam :: (Read a) => [Text] -> Text -> Maybe (UrlParam s a)
lookupUrlParam xs s = case elemIndex s xs of
    Just i -> Just $ UrlParam $ read $ unpack $ xs !! (i+1)
    Nothing -> Nothing

instance (Read a, KnownSymbol s) => RequestDerivable (UrlParam s a) where
  extract req = return $ case lookupUrlParam (pathInfo req) (pack $ symbolVal (Proxy :: Proxy s)) of
    Just x -> Right x
    Nothing -> Left $ status404

class Handler a where
  execute :: Request -> a -> IO Response

instance (Handler b, RequestDerivable a) => Handler (a -> b) where
  execute r fn = do
    v <- extract r
    case v of
      Right x -> execute r (fn x)
      Left s -> return $ responseLBS s [] ""

instance {-# OVERLAPPABLE #-} (ToResponse format a) => Handler (ResponseFormat format (Endpoint a)) where
  execute r (ResponseFormat a) = do
    v <- a
    return $ toResponse v (Proxy :: Proxy format)

instance (ToResponse format a, Handler (ResponseFormat formats (Endpoint a))) => Handler (ResponseFormat (format:formats) (Endpoint a)) where
  execute r (ResponseFormat a) = if doesRequestMatchContentType r (Proxy :: Proxy format)
    then execute r (ResponseFormat a :: ResponseFormat format (Endpoint a))
    else execute r (ResponseFormat a :: ResponseFormat formats (Endpoint a))
    where
      doesRequestMatchContentType :: Request -> Proxy format -> Bool
      doesRequestMatchContentType _ _ = True

instance Handler (ResponseFormat '[] (Endpoint a)) where
  execute r _ = return $ responseLBS status415 [] "Unsupported media type"

instance Convertable (Endpoint a) (ResponseFormat format (Endpoint a)) where
  convert a = ResponseFormat $ a

data HandlerStack where
  EmptyStack :: HandlerStack
  AddToStack :: forall a. (Handler a) => a -> HandlerStack -> HandlerStack

class ToHandlerStack a where
  toHandlerStack :: a -> HandlerStack

instance {-# OVERLAPPING #-} (ToHandlerStack b) => ToHandlerStack (a <|> b) where
  toHandlerStack (HandlerPair a b) = AddToStack a (toHandlerStack b)

instance (Handler a) => ToHandlerStack a where
  toHandlerStack a = AddToStack a EmptyStack

class Encodable format a where
  encode :: a -> Proxy format -> ByteString
  
class ToResponse format a where
  toResponse :: a -> Proxy format -> Response

class Convertable a b where
  convert :: a -> b

class ContentType a where
  toContentType :: Proxy a -> ByteString

instance {-# OVERLAPPABLE #-} Convertable a a where
  convert = id

instance {-# OVERLAPPABLE #-} (Convertable a b) => Convertable (b -> c) (a -> c) where
  convert fn = fn.convert

instance {-# OVERLAPPING #-} (Convertable b c) => Convertable (a -> b) (a -> c) where
  convert fn = convert.fn

instance {-# OVERLAPPING #-} (Convertable c a, Convertable b d) => Convertable (a -> b) (c -> d) where
  convert fn = convert.fn.convert

instance Convertable (UrlParam s a) a where
  convert (UrlParam a) = a

instance (Convertable a c, Handler c, Convertable b d) => Convertable (a <|> b) (c <|> d) where
  convert (Pair a b) = HandlerPair (convert a) (convert b)

instance {-# OVERLAPPABLE #-} (Encodable format a, ContentType format) => ToResponse format a where
  toResponse a p = responseLBS status200 [] $ fromStrict $ encode a p

data a <|> b where
  Pair :: a -> b -> a <|> b
  HandlerPair :: (Handler a) => a -> b -> a <|> b

infixr 8 <|>

type family RightServer a :: * where
  RightServer (a <|> b) = b

type family ToHandlers a where
  ToHandlers (a <|> b) = (ExtractHandler a) <|> (ToHandlers b)
  ToHandlers a = (ExtractHandler a)

(<|>) :: a -> b -> (a <|> b)
(<|>) a b = Pair a b

type family ChangeEndpoint a where
  ChangeEndpoint (a <|> b) = (ChangeEndpoint a) <|> (ChangeEndpoint b)
  ChangeEndpoint (a -> b) = (a -> ChangeEndpoint b)
  ChangeEndpoint (Endpoint a) = Endpoint a
  ChangeEndpoint (m a) = Endpoint a

class ToEndpoint m1 m2 e where
  toEndpoint :: e -> m1 a -> m2 a

instance ToEndpoint Endpoint Endpoint () where
  toEndpoint _ = id

class ToEndpoints a e where
  toEndpoints :: e -> a -> ChangeEndpoint a

instance {-# OVERLAPPABLE #-} (ToEndpoint m1 Endpoint e, ChangeEndpoint (m1 a) ~ Endpoint a) => ToEndpoints (m1 a) e where
  toEndpoints e a = toEndpoint e a

instance (ToEndpoints b e) => ToEndpoints (a -> b) e where
  toEndpoints e fn = \x -> toEndpoints e (fn x)

instance (ToEndpoints a e, ToEndpoints b e) => ToEndpoints (a <|> b) e where
  toEndpoints e (Pair a b) = Pair (toEndpoints e a) (toEndpoints e b)
  toEndpoints e (HandlerPair a b) = Pair (toEndpoints e a) (toEndpoints e b)

serve
  :: forall a b c e m.
  ( ManySymbolLists (ExtractUrlList a)
  , ToHandlerStack (ToHandlers a)
  , ExtractTC b ~ m
  , ToEndpoints b e
  , ToEndpoint m Endpoint e
  , Convertable (ChangeEndpoint b) (ToHandlers a))
  => Proxy a
  -> b
  -> e
  -> Application
serve p h env = application $ makeRoutes $ (toSymbolLists $ (Proxy :: Proxy (ExtractUrlList a))) 
  where
    application !routes r respond =
      case lookupRequest r routes of
        Just n -> processRequest r (toHandlerStack $ (convert (toEndpoints env h) :: (ToHandlers a))) n >>= respond
        Nothing -> respond $ responseLBS status404 [] $ "Path does not exist."
    processRequest :: Request -> HandlerStack -> Int -> IO Response
    processRequest r (AddToStack h _) 0 = execute r h
    processRequest r (AddToStack h b) c = processRequest r b (c-1)
