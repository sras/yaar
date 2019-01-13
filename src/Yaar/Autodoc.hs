{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}
{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language MonoLocalBinds #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-} -- for a possible ghc bug that throws this warning for 'ExtractType b ~ m' constraint in serve function

module Yaar.Autodoc
  ( RouteInfoSegment(..)
  , RouteInfoPara(..)
  , RouteInfo
  , ToApiDoc(..)
  , toSimpleRouteInfo
  , RouteInfoSimple(..)
  , Schema(..)
  , RouteComponent(..)
  , ToYaarSchema(..)
  , YaarSchema(..)
  , SimpleSchema(..)
  , simpleSchema
  )
where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Proxy
import Yaar.Core
import GHC.TypeLits as TL
import Data.List
import Data.Swagger
import Data.ByteString.Char8 (unpack)

instance ToSchema RouteInfoSimple where
  declareNamedSchema a = pure $ NamedSchema (Just "RouteInfoSimple") mempty

data RouteComponent = TextSegment String | ParaSegment String YaarSchema deriving (Generic)

data SimpleSchema
  = SimpleSchema
      { schemaTypeName :: String
      , schemaProperties :: [(String, YaarSchema)]
      } deriving (Generic)

data YaarSchema = Simple SimpleSchema | Full Schema deriving (Generic)

simpleSchema :: String -> [(String, YaarSchema)] -> YaarSchema
simpleSchema n l = Simple $ SimpleSchema { schemaTypeName = n, schemaProperties = l }

instance Aeson.ToJSON SimpleSchema where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Aeson.ToJSON YaarSchema where
  toJSON v = case v of
    Full s -> Aeson.toJSON s
    Simple s -> Aeson.toJSON s

class ToYaarSchema a where
  toYaarSchema :: Proxy a -> YaarSchema

instance {-# OVERLAPPABLE #-} (ToSchema a) => ToYaarSchema a where
  toYaarSchema a = Full $ toSchema a

data RouteInfoPara a b =
  RouteInfo
    { routePath :: a
    , routeRequestBody :: Maybe YaarSchema
    , routeRequestBodyFormat :: [b]
    , routeRequestHeaders :: [(String, YaarSchema)]
    , routeMethod :: Maybe String
    , routeOutputFormat :: [b]
    , routeOutput :: Maybe YaarSchema
    , routeQuery :: [(String, YaarSchema)]
    } deriving (Generic)

type RouteInfo = RouteInfoPara [RouteComponent] ByteString

type RouteInfoSimple = RouteInfoPara String String

instance Aeson.ToJSON RouteInfoSimple where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

toStringRouteComponent :: RouteComponent -> String
toStringRouteComponent (TextSegment s) = s
toStringRouteComponent (ParaSegment s _) = "{" ++ s ++ "}"

toSimpleRouteInfo :: RouteInfo -> RouteInfoSimple
toSimpleRouteInfo r
  = r { routePath = intercalate "/" $ toStringRouteComponent <$> (routePath r)
      , routeOutputFormat = unpack <$> routeOutputFormat r
      , routeRequestBodyFormat = unpack <$> routeRequestBodyFormat r
      }

emptyRouteInfo :: RouteInfo 
emptyRouteInfo =
  RouteInfo
    { routePath = []
    , routeRequestBody = Nothing
    , routeRequestBodyFormat = []
    , routeRequestHeaders = []
    , routeMethod = Nothing
    , routeOutputFormat = []
    , routeOutput = Nothing
    , routeQuery = []
    }

class RouteInfoSegment a where
  addRouteInfo :: Proxy a -> (RouteInfo -> RouteInfo)

instance (ToYaarSchema a, Method method) => RouteInfoSegment (method '[] a) where
  addRouteInfo _ = (\x -> x { routeOutput = Just $ toYaarSchema (Proxy :: Proxy a), routeMethod = Just (toMethodName (Proxy :: Proxy method)), routeOutputFormat = [] })

instance {-# OVERLAPPABLE #-} (ContentType format, ToYaarSchema a, RouteInfoSegment (method xs a)) => RouteInfoSegment (method (format:xs) a) where
  addRouteInfo _ =
    (\x -> 
        let
          ct =
            case getContentType (Proxy :: Proxy format) of
              Just x -> x
              Nothing -> "-"
          rx = addRouteInfo (Proxy :: Proxy (method xs a)) x
        in rx { routeOutput  = Just $ toYaarSchema (Proxy :: Proxy a), routeOutputFormat = ct: routeOutputFormat rx })

instance (KnownSymbol a) => RouteInfoSegment a where
  addRouteInfo a = (\x -> x { routePath = (TextSegment $ symbolVal (Proxy :: Proxy a)):routePath x })

instance (KnownSymbol a, ToYaarSchema b) => RouteInfoSegment (UrlParam a b) where
  addRouteInfo a = (\x -> x { routePath = (ParaSegment (symbolVal (Proxy :: Proxy a)) $ toYaarSchema (Proxy :: Proxy b) ):routePath x })

instance (RouteInfoSegment a, RouteInfoSegment b) => RouteInfoSegment (a :> b) where
  addRouteInfo _ = (addRouteInfo (Proxy :: Proxy a)) . (addRouteInfo (Proxy :: Proxy b))

class ToApiDoc a where
  toApiDocList :: Proxy a -> [RouteInfo]

instance {-# OVERLAPPABLE #-}  (RouteInfoSegment a) => ToApiDoc a  where
  toApiDocList _ = [addRouteInfo (Proxy :: Proxy a) $ emptyRouteInfo]

instance (RouteInfoSegment a, ToApiDoc b) => ToApiDoc (a <|> b) where
  toApiDocList _ = addRouteInfo (Proxy :: Proxy a) emptyRouteInfo : (toApiDocList (Proxy :: Proxy b))
