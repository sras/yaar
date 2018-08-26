{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}

import Yaar
import Yaar.Routing
import Test.Hspec
import Network.HTTP.Types (hAccept)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Internal
import Network.Wai (defaultRequest)
import Network.Wai.Test
import Control.Exception
import Control.DeepSeq (force)
import Data.Text (Text, pack)
import Data.Proxy
import GHC.Generics
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

type TestServer =  "home" :> "profile" :> "bio" :> (GET '[PlainText, HTML] String)
               <|> "home" :> "profile" :> "orders" :> (GET PlainText Text)
               <|> "home" :> "profile" :> "resume" :> (GET [PlainText, JSON] Resume)

data Resume = Resume { name :: Text, otherStuff :: Text } deriving (Generic, Show)

resumePlainEncoding :: ByteString
resumePlainEncoding = "Plain text encoding of resume"

instance ToJSON Resume where
  toJSON _ = object [ "json-of" .= ("resume" :: Text) ]

instance FromJSON Resume where
  parseJSON = genericParseJSON defaultOptions

instance Encodable PlainText Resume where
  encode v _ = resumePlainEncoding
--

server =  handlerBio
      <|> handlerOrders
      <|> handlerResume

handlerBio :: IO String
handlerBio = return $ "Index"

handlerOrders :: IO Text
handlerOrders = return "Orders"

handlerResume :: IO Resume
handlerResume = return $ Resume { name = "Jane Doe", otherStuff = "lorem ipsm" }

api :: Proxy TestServer
api = Proxy

app = serve api server ()

main :: IO ()
main = hspec $ do
  describe "basic-behavior" $ do
    it "should generate the right response - 1" $ do
      let session = request (setPath defaultRequest "/home/profile/bio")
      response <- runSession session app
      simpleBody response `shouldBe` "Index"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should generate the right response - 2" $ do
      let session = request (setPath defaultRequest "/home/profile/orders")
      response <- runSession session app
      simpleBody response `shouldBe` "Orders"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should generate the right response or matching request content type" $ do
      let
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "text/plain")] }) "/home/profile/resume")
          assertContentType "text/plain" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` (LB.fromStrict resumePlainEncoding)
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should generate the right response or matching request content type" $ do
      let 
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "application/json")] }) "/home/profile/resume")
          assertContentType "application/json" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "{\"json-of\":\"resume\"}"
      (statusCode.simpleStatus) response `shouldBe` 200
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
