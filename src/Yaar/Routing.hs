{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}

module Yaar.Routing (Routes, lookupRequest, makeRoutes) where

import Data.Text as T (unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Map.Strict as DM (Map, empty, lookup, insert)
import Network.Wai ( Request , requestMethod , pathInfo)
import Control.DeepSeq (NFData)
import GHC.Generics

data RouteSegment = MethodSegment Text | ParamSegment | UrlSegment Text deriving (Eq, Ord, Generic, NFData)

data SegmentsWithIndex = SegmentsWithIndex Int [RouteSegment]

data Routes = RouteTree Int (Map RouteSegment Routes) deriving (Show, NFData, Generic)
-- Route with an Precedence field

instance Show RouteSegment where
  show (UrlSegment x) = unpack x
  show ParamSegment = "::param::"
  show (MethodSegment x) = unpack x

toRouteSegments :: [Text] -> Int -> SegmentsWithIndex
toRouteSegments (x:xs) p = SegmentsWithIndex p $ (MethodSegment x): (toUrlSegment <$> xs)
  where
    toUrlSegment :: Text -> RouteSegment
    toUrlSegment "::param::" = ParamSegment
    toUrlSegment y = UrlSegment y
toRouteSegments [] _ = error "Route needs at least a METHOD segment"

makeRoutes :: [[Text]] -> Routes
makeRoutes !x = foldl insertRoute (RouteTree 0 $ DM.empty) $ (zipWith toRouteSegments (putMethodInfront <$> x) [0..])
  where
  putMethodInfront :: [Text] -> [Text]
  putMethodInfront y = (last y):(init y)
  insertRoute ::Routes -> SegmentsWithIndex -> Routes
  insertRoute (RouteTree _ _) (SegmentsWithIndex p []) = emptyTree p
  insertRoute (RouteTree o map_) (SegmentsWithIndex p (r:rs)) = case DM.lookup r map_ of
    Nothing -> RouteTree o $ DM.insert r (insertRoute (emptyTree p) (SegmentsWithIndex p rs)) map_
    Just submap -> RouteTree o $ DM.insert r (insertRoute submap (SegmentsWithIndex p rs)) map_
  emptyTree p = RouteTree p DM.empty

lookupRequest :: Request -> Routes -> Maybe Int
lookupRequest r !routes = let
  SegmentsWithIndex _ routeSegments = toRouteSegments ((decodeUtf8 $ requestMethod r):(pathInfo r)) 0
  in lookupRoute routes routeSegments

lookupRoute :: Routes -> [RouteSegment] -> Maybe Int
lookupRoute (RouteTree p _) [] = Just p
lookupRoute (RouteTree _ map_) (r:rs) =
  -- Look for an exact match for the segment 'r' and for a wild card match
  -- if both are found use the one with a higher precedence
  -- or else just use the available one.
  -- It none of them are found return Nothing indicating a route lookup failure
  case (DM.lookup ParamSegment map_, DM.lookup r map_) of
    (Just x@(RouteTree px _), Just w@(RouteTree pw _)) -> if px < pw then lookupRoute x rs else lookupRoute w rs
    (Just x, Nothing) -> lookupRoute x rs
    (Nothing, Just x) -> lookupRoute x rs
    (Nothing, Nothing) -> Nothing
