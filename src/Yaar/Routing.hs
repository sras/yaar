{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}

module Yaar.Routing (Routes, lookupRequest, lookupRoute, makeRoutes, printRoutes) where

import Data.Text as T (unpack, Text, intercalate, concat)
import Data.Text.Encoding (decodeUtf8)
import Data.Map.Strict as DM (Map, toList, empty, lookup, insert)
import Network.Wai ( Request , requestMethod , pathInfo)
import Control.DeepSeq (NFData)
import GHC.Generics

data RouteSegment = MethodSegment Text | ParamSegment | UrlSegment Text | Terminator  deriving (Eq, Ord, Generic, NFData)

data Routes = RouteTree (Map RouteSegment (Int, Routes)) | RouteEnd deriving (Show, NFData, Generic)
-- Route with an Precedence field
--

lookupRequest :: Request -> Routes -> Maybe Int
lookupRequest r !routes = let
  routeSegments = ((MethodSegment $ decodeUtf8 $ requestMethod r):(UrlSegment <$> pathInfo r))
  in lookupRoute routes routeSegments
-- 
lookupRoute :: Routes -> [RouteSegment] -> Maybe Int
lookupRoute (RouteTree map_) [] =
  case DM.lookup Terminator map_ of
    Just (i, _) -> Just i
    Nothing -> Nothing
lookupRoute (RouteTree map_) (r:rs) =
  -- Look for an exact match for the segment 'r' and for a wild card match
  -- if both are found use the one with a higher precedence
  -- or else just use the available one.
  -- It none of them are found return Nothing indicating a route lookup failure
  case (DM.lookup ParamSegment map_, DM.lookup r map_) of
    (Just (px, x), Just (pw, w)) -> if px < pw then lookupRoute x rs else lookupRoute w rs
    (Just (_, x), Nothing) -> lookupRoute x rs
    (Nothing, Just (_, x)) -> lookupRoute x rs
    (Nothing, Nothing) -> Nothing
lookupRoute RouteEnd  _ = error "Routend should never be encountered during route segment look up"

makeRoutes :: [[Text]] -> Routes
makeRoutes !x = foldl insertRoute (RouteTree DM.empty) $ (zipWith toRouteSegments (putMethodInfront <$> x) [0..])
  where
  putMethodInfront :: [Text] -> [Text]
  putMethodInfront y = (last y):(init y)
  insertRoute :: Routes -> [(Int, RouteSegment)] -> Routes
  insertRoute (RouteTree _) [] = error "Cannot insert an empty route"
  insertRoute RouteEnd _ = error "Cannot insert more segments to terminated route"
  insertRoute (RouteTree map_) ((i, r):[]) =
    case DM.lookup r map_ of
      Nothing -> RouteTree $ DM.insert r (i, (terminator i (RouteTree DM.empty))) map_
      Just (_, submap) -> RouteTree $ DM.insert r (i, (terminator i submap)) map_
  insertRoute (RouteTree map_) ((i, r):rs) =
    case DM.lookup r map_ of
      Nothing -> RouteTree $ DM.insert r (i, (insertRoute emptyTree rs)) map_
      Just (_, submap) -> RouteTree $ DM.insert r (i, (insertRoute submap rs)) map_
  emptyTree = RouteTree DM.empty
  terminator i (RouteTree sub) = RouteTree (DM.insert Terminator (i, RouteEnd) sub)
  terminator _ RouteEnd  = error "Cannot add terminator to already terminated route"

toRouteSegments :: [Text] -> Int -> [(Int, RouteSegment)]
toRouteSegments (x:xs) p = (p, MethodSegment x): (toUrlSegment <$> xs)
  where
    toUrlSegment :: Text -> (Int, RouteSegment)
    toUrlSegment "::param::" = (p, ParamSegment)
    toUrlSegment y = (p, UrlSegment y)
toRouteSegments [] _ = error "Route needs at least a METHOD segment"

printRoutes :: Routes -> T.Text
printRoutes r = T.intercalate "\n" $ Prelude.concat $ printInnerRoutes <$> routesToList r
  where
  segmentToText :: RouteSegment -> Text
  segmentToText (MethodSegment s) = s
  segmentToText ParamSegment = "::param::"
  segmentToText Terminator = "::terminator::"
  segmentToText (UrlSegment s) = s
  routesToList :: Routes -> [(RouteSegment, (Int, Routes))]
  routesToList (RouteTree map_) = toList map_
  routesToList RouteEnd = []
  prependSegment :: RouteSegment -> Text -> Text
  prependSegment rs s = T.concat [segmentToText rs, "/", s]
  printInnerRoutes :: (RouteSegment, (Int, Routes)) -> [T.Text]
  printInnerRoutes (s, (_, routes)) =
    case (routesToList routes) of
      [] ->
        [segmentToText s]
      routeList -> 
        (prependSegment s) <$> (Prelude.concat $ printInnerRoutes <$> routeList)
-- 
instance Show RouteSegment where
  show (UrlSegment x) = unpack x
  show ParamSegment = "::param::"
  show (MethodSegment x) = unpack x
  show Terminator = "::terminator::"
