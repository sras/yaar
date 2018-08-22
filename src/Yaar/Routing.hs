{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}

module Yaar.Routing (Routes, lookupRequest, makeRoutes) where

import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Map.Strict as DM (Map, empty, lookup, insert)
import Network.Wai ( Request , requestMethod , pathInfo)

data RouteSegment = MethodSegment Text | ParamSegment | UrlSegment Text deriving (Eq, Ord, Show) 

data Routes = RouteTree (Map RouteSegment Routes) | RouteIndex Int deriving (Show)

toRouteSegments :: [Text] -> [RouteSegment]
toRouteSegments (x:xs) = (MethodSegment x): (toUrlSegment <$> xs)
  where
    toUrlSegment :: Text -> RouteSegment
    toUrlSegment "::param::" = ParamSegment
    toUrlSegment x = UrlSegment x

makeRoutes :: [[Text]] -> Routes
makeRoutes !x = foldl insertRoute (RouteTree $ DM.empty) $ zip (toRouteSegments . putMethodInfront <$> x) [0..]
  where
  putMethodInfront :: [Text] -> [Text]
  putMethodInfront x = (last x):(init x)
  insertRoute :: Routes -> ([RouteSegment], Int) -> Routes
  insertRoute (RouteTree map)  ([s], idx) = case lookupSegment s map of
    Nothing -> RouteTree $ DM.insert s (RouteIndex idx) map
    Just _ -> error "Overlapping routes found!"
  insertRoute (RouteTree map) ((r:rs), idx) = case lookupSegment r map of
    Just m -> RouteTree $ DM.insert r (insertRoute m (rs, idx)) map
    Nothing -> RouteTree $ DM.insert r (insertRoute (RouteTree DM.empty) (rs, idx)) map
  lookupSegment :: RouteSegment -> Map RouteSegment Routes -> Maybe Routes
  lookupSegment r m = case DM.lookup r m of
    Just a -> Just a
    Nothing -> DM.lookup ParamSegment m

lookupRequest :: Request -> Routes -> Maybe Int
lookupRequest r !routes = let
  routeSegments = toRouteSegments $ (decodeUtf8 $ requestMethod r):(pathInfo r)
  in lookUp routes routeSegments
  where
    lookUp :: Routes -> [RouteSegment] -> Maybe Int
    lookUp (RouteIndex i) [] = Just i
    lookUp routes@(RouteTree map) [] = Nothing
    lookUp routes@(RouteTree map) (r:rs) = case DM.lookup r map of
        Just x -> lookUp x rs
        Nothing -> case DM.lookup ParamSegment map of
          Just x -> lookUp x rs
          Nothing -> Nothing

