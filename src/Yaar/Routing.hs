{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}

module Yaar.Routing (Routes, lookupRequest, lookupRoute, makeRoutes, printRoutes) where

import Data.Text as T (unpack, Text, intercalate, concat, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Map.Strict as DM (Map, toList, empty, lookup, insert)
import Network.Wai ( Request , requestMethod , pathInfo)
import Control.DeepSeq (NFData)
import GHC.Generics

data RouteSegment = MethodSegment Text | ParamSegment | UrlSegment Text | Terminator  deriving (Eq, Ord, Generic, NFData)

data Routes = RouteTree (Map RouteSegment (Int, Routes)) | RouteEnd deriving (Show, NFData, Generic)
-- Route with an Precedence field
--

routingLog :: Maybe (Text -> IO ()) -> Text -> IO ()
routingLog logger l =
  case logger of
    Just lgr -> lgr l
    Nothing -> pure ()

lookupRequest :: Request -> Routes -> Maybe (Text -> IO ()) -> IO (Maybe Int)
lookupRequest r !routes mlogger = let
  routeSegments = ((MethodSegment $ decodeUtf8 $ requestMethod r):(UrlSegment <$> pathInfo r))
  in do
      routingLog mlogger $ T.concat ["Trying to match route: ", T.pack $ show routeSegments]
      routingLog mlogger $ "App Routes:"
      routingLog mlogger $ "---"
      routingLog mlogger $ printRoutes routes
      routingLog mlogger $ "---"
      lookupRoute (routingLog mlogger) routes routeSegments 

lookupRoute :: (Text -> IO ()) -> Routes -> [RouteSegment] -> IO (Maybe Int)
lookupRoute logger rts rss = case rts of
  RouteEnd  -> error "Routend should never be encountered during route segment look up"
  RouteTree map_ -> 
    case rss of
        [] -> pure $ case DM.lookup Terminator map_ of
                Just (i, _) -> Just i
                Nothing -> Nothing
        (r:rs) -> do
          logger $ T.concat ["Matching segment ", (pack $ show r)]
          -- Look for an exact match for the segment 'r' and for a wild card match
          -- if both are found use the one with a higher precedence
          -- or else just use the available one.
          -- It none of them are found return Nothing indicating a route lookup failure
          case (DM.lookup ParamSegment map_, DM.lookup r map_) of
            (Just (px, x), Just (pw, w)) -> do
              logger $ T.concat ["Found matches for a parameter and '", pack $ show r ,"'"]
              if px < pw
                then do
                  logger $ T.concat ["Selecting matches with higher priority: ParamSegment"]
                  lookupRoute logger x rs
                else do
                  logger $ T.concat ["Selecting matches with higher priority: ", pack $ show r]
                  lookupRoute logger w rs
            (Just (_, x), Nothing) -> do
              logger $ T.concat ["Found exact match for: ", pack $ show r]
              lookupRoute logger x rs
            (Nothing, Just (_, x)) -> do
              logger $ T.concat ["Found a segment match"]
              lookupRoute logger x rs
            (Nothing, Nothing) -> do
              logger $ T.concat ["No match found. Look up failed."]
              pure Nothing

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
