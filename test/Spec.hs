{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}

import Yaar
import Yaar.Routing
import Test.Hspec
import Network.HTTP.Types (hAccept)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Internal
import Network.Wai (Application, defaultRequest)
import Network.Wai.Test
import Control.Exception
import Control.DeepSeq (force)
import Data.Text (Text, pack, unpack)
import Data.Proxy
import GHC.Generics
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

type TestServer =  "home" :> "profile" :> "bio" :> (GET '[PlainText, HTML] String)
               <|> "home" :> "profile" :> "orders" :> (GET PlainText Text)
               <|> "home" :> "profile" :> "resume" :> (GET '[PlainText, JSON] Resume)
               <|> "home" :> "profile" :> "resume" :> "add" :> ReqBody JSON Resume :> (POST '[JSON] Resume)
               <|> "home" :> "post" :> UrlParam "id" Text :> (GET PlainText String)
               <|> "request" :> "with" :> "header" :> UrlParam "id" Text :> (GET PlainText (ResponseHeader ["custom-header-1", "custom-header-2"] String))
               <|> "request" :> "with" :> "input" :> "header" :> RequestHeader "input-header" Text :> GET '[PlainText] String
               <|> "request" :> "with" :> "no" :> "content" :> GET '[] NoContent

data Resume = Resume { name :: Text } deriving (Generic, Show)

instance ToJSON Resume where
  toJSON = genericToJSON defaultOptions

instance FromJSON Resume where
  parseJSON = genericParseJSON defaultOptions

instance Encodable PlainText Resume where
  encode v _ = encodeUtf8 $ pack $ show v

server =  handlerBio
      <|> handlerOrders
      <|> handlerResume
      <|> handlerAddResume
      <|> handlerPost
      <|> handlerWithHeader
      <|> handlerHeaderInput
      <|> handlerWithNoContent

handlerBio :: IO String
handlerBio = return $ "Index"

handlerOrders :: IO Text
handlerOrders = return "Orders"

handlerResume :: IO Resume
handlerResume = return $ Resume { name = "Jane Doe" }

handlerAddResume :: Resume -> IO Resume
handlerAddResume r = return $ r

handlerPost :: Text -> IO String
handlerPost postId = return $ "Post " ++ (unpack postId)

handlerHeaderInput :: Text -> IO String
handlerHeaderInput headerInput = return $ "Header value = " ++ (unpack headerInput)

handlerWithNoContent :: IO NoContent
handlerWithNoContent = do
  return $ NoContent

handlerWithHeader :: Text -> IO (ResponseHeader '["custom-header-1", "custom-header-2"] String)
handlerWithHeader id = return $
  addHeader (Proxy :: Proxy "custom-header-1") "header-1-value" $
  addHeader (Proxy :: Proxy "custom-header-2") "header-2-value" $
  "Header Request " ++ (unpack id)

api :: Proxy TestServer
api = Proxy

app :: Application
app = serve api server ()

type TestServer2 =  "home" :> "profile" :> "bio" :> (GET '[PlainText, HTML] String)

server2 =  handlerBio2

api2 :: Proxy TestServer2
api2 = Proxy

handlerBio2 :: Maybe String -- A handler that runs in the Maybe
handlerBio2 = return $ "Index"

instance RunnableTo Maybe IO () where -- This instance enables the handler to run in Maybe
  runTo _ (Just a)  = return a
  runTo _ Nothing = error "No result for this endpoint"

anotherMonadApp :: Application
anotherMonadApp = serve api2 server2 ()

main :: IO ()
main = hspec $ do
  describe "basic-behavior-in-another-monad" $ do
    it "should generate the right response - 1" $ do
      let session = request (setPath defaultRequest "/home/profile/bio")
      response <- runSession session anotherMonadApp
      simpleBody response `shouldBe` "Index"
      (statusCode.simpleStatus) response `shouldBe` 200
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
      simpleBody response `shouldBe` "Resume {name = \"Jane Doe\"}"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should return error for not requesting supported content type" $ do
      let
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "blah!")] }) "/home/profile/resume")
          return r
      response <- runSession session app
      (statusCode.simpleStatus) response `shouldBe` 415
    it "should generate the right response or matching request content type" $ do
      let 
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "application/json")] }) "/home/profile/resume")
          assertContentType "application/json" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "{\"name\":\"Jane Doe\"}"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should generate the html response for wildcard content type" $ do
      let 
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "*/*")] }) "/home/profile/bio")
          assertContentType "text/plain" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "Index"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should pass the param from url to handler" $ do
      let 
        session = do
          r <- request (setPath (defaultRequest { requestHeaders = [(hAccept, "application/json")] }) "/home/post/id/21")
          assertContentType "text/plain" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "Post 21"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should respond with expected headers" $ do
      let 
        session = do
          r <- request (setPath defaultRequest "/request/with/header/id/21")
          assertContentType "text/plain" r
          assertHeader "custom-header-1" "header-1-value" r
          assertHeader "custom-header-2" "header-2-value" r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "Header Request 21"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should respond back with posted item" $ do
      let 
        session = do
          let request = (setPath (defaultRequest { requestHeaders = [(hAccept, "application/json")],  requestMethod = "POST"}) "/home/profile/resume/add")
          r <- srequest $ SRequest request $ Data.Aeson.encode $ Resume "John Doe"
          assertContentType "application/json" $ r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "{\"name\":\"John Doe\"}"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should respond back with input in header item" $ do
      let 
        session = do
          let req = (setPath (defaultRequest { requestHeaders = [("input-header", "Test input String")] }) "/request/with/input/header")
          r <- request $ req
          assertContentType "text/plain" $ r
          return r
      response <- runSession session app
      simpleBody response `shouldBe` "Header value = Test input String"
      (statusCode.simpleStatus) response `shouldBe` 200
    it "should respond back with empty body" $ do
      let 
        session = do
          let req = (setPath defaultRequest "/request/with/no/content")
          r <- request req
          return r
      response <- runSession session app
      simpleBody response `shouldBe` ""
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
