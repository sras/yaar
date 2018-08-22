{-# Language OverloadedStrings #-}

import Yaar.Routing
import Test.Hspec
import Network.Wai.Internal
import Network.Wai (defaultRequest)
import Control.Exception
import Control.DeepSeq (force)
import Data.Text (Text)

main :: IO ()
main = hspec $ do
  describe "routing" $ do
    it "should match when there is a param route but no exact match" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg1", "seg2", "seg3"] }
        routes = makeRoutes
          [ [ "seg1"
          , "seg4"
          , "seg3"
          , "GET" ]
          , [ "seg1"
          , "::param::"
          , "seg3"
          , "GET"
          ]
          ]
      in lookupRequest request routes `shouldBe` (Just 1)
    it "should return param route match even when there is an exact match of lower precedence" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg1", "seg4", "seg3"] }
        routes = makeRoutes
          [ [ "seg1"
          , "seg4"
          , "::param::"
          , "GET"
          ]
          , [ "seg1"
          , "seg4"
          , "seg3"
          , "GET"
          ]
          ]
      in lookupRequest request routes `shouldBe` (Just 0)
    it "should return exact route match even when there a param route of lower precedence" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg1", "seg4", "seg3"] }
        routes = makeRoutes
          [ [ "seg1"
          , "seg4"
          , "seg3"
          , "GET" ]
          , [ "seg1"
          , "::param::"
          , "seg3"
          , "GET"
          ]
          ]
      in lookupRequest request routes `shouldBe` (Just 0)
    it "should match when incoming route exactly match with one in routes" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg1", "seg4", "seg3"] }
        routes = makeRoutes
          [ [ "seg1"
            , "seg4"
            , "seg3"
            , "GET"
            ]
          ]
      in lookupRequest request routes `shouldBe` Just 0
    it "should return Nothing when there is no matching route" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg1", "seg2", "seg2"] }
        routes = makeRoutes
          [ [ "seg1"
          , "seg4"
          , "seg3"
          , "GET" ]
          , [ "seg1"
          , "::param::"
          , "seg3"
          , "GET"
          ]
          ]
      in lookupRequest request routes `shouldBe` Nothing
    it "should match correct one when there are many routes defined" $
      let
        request = defaultRequest { requestMethod = "GET", pathInfo = ["seg5", "seg6", "seg7"] }
        routes = makeRoutes
          [ [ "seg1"
            , "seg4"
            , "seg3"
            , "GET"
            ]
          , [ "seg5"
            , "seg6"
            , "seg7"
            , "GET"
            ]
          ]
      in lookupRequest request routes `shouldBe` Just 1
