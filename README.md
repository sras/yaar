# Yaar 

~*~ Yaar means friend ~*~

Yaar is an attempt to independently implement an interface that loosly resembels that of Haskell's `Servant` web framework.
If you are not aware of `Servant`, what this interface enables you is the auto generation of api spec that is gauranteed (by the type checker) to be in sync with the actual implementation.

### How to make a web application using Yaar?

Please see the `test/Spec.hs` file in this repo to see a sample app.

Each endpoint/route in a Yaar application has its own type. This type specifies the kinds of inputs the route can accept, the types and possible encodings for those inputs,  the output type and the possible encodings for the output.
The type for a route can look something like this.

```
"home" :> "profile" :> "bio" :> (GET '[HTML] Text)
```

This type represents an endpoint that will be available via a GET request at url "home/profile/bio" that returns a `Text` value in `HTML` format.
The `'[HTML]` is a type level list, that can contain more than one type, and so a single endpoint can return data in multiple formats.
For example, the following endpoint can return data in both HTML and XML formats.

```
"home" :> "profile" :> "bio" :> (GET '[HTML, XML] Text)
```

Note that while `HTML` is shipped with Yaar, `XML` is not. Anyway, the important point to note here is that the handler does not have to
bother with the actual format that is contained in the response, and Yaar only expect handler to return a value of type `Text`. The final encoding
and constructor of the response is handled by itself. It does this by looking at the `Accept` header in the request, and encodes the response in a matching format.

Multiple endpoints can be combined using the `<|>` type operator forming the type of the complete app.

```
type AppType
   =  "home" :> "profile" :> "bio" :> (GET '[HTML] Text)
  <|> "home" :> "profile" :> "contact" :> (GET '[HTML] Text)
```

Now, you have to implement actual handlers for these endpoints. By default Yaar handlers run in `IO`. So both of the handlers for our App will be functions of type `IO Text`. Once we have the handler functions, we combine them using the `<|>` function to make the complete app as follows.

```
appHandlers :: Server AppType IO
appHandlers
  = bioHandler
 <|> contactHandler
```

You can see the type which represents the entire app (`AppType` here) referenced here in the signature. Thus if there is a mismatch between your endpoint types and the handlers, you will get a type error here.

And we finally make a `wai` `Application` from our handlers and handler types using the `serve` function as shown below.

```
app :: Application
app = serve (Proxy :: Proxy AppType) appHandlers (\_ _ -> pure ()) Nothing
```

The last argument here is an function that creates an environment derived from the incoming request, for the handlers to execute. By default our handlers run in IO, and does not require an environment to run and thus we are passing the empty tuple as the environment.


```
{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}

import Yaar
import Data.Text
import Data.Proxy

type AppType
   =  "home" :> "profile" :> "bio" :> (GET '[HTML] Text)
  <|> "home" :> "profile" :> "contact" :> (GET '[HTML] Text)

bioHandler :: IO Text
bioHandler = pure "This is my Bio"

contactHandler :: IO Text
contactHandler = pure "This is my contact info"

appHandlers :: Server AppType IO
appHandlers
  = bioHandler
 <|> contactHandler

app :: Application
app = serve (Proxy :: Proxy AppType) appHandlers (\_ _ -> pure ()) Nothing

main = run 4000 app
```

That is it.

### How to add HTTP headers to a response

If you want to add a header with name "MyHeaderName" with value "MyHeaderValue", to an endpoint that return a value of type `String`

that is, let our endpoint without the header be

```
"request" :> "with" :> "header" :> (GET '[PlainText] String)
```

To change this type to accomodate our header, modify the `String` in the last segment to wrap in the `ResponseHeader` type.
that is, change `GET '[PlainText] String` to `GET '[PlainText] (ResponseHeader ["MyHeaderName"] String)`.


```
"request" :> "with" :> "header" :> (GET '[PlainText] (ResponseHeader ["MyHeaderName"] String))
```

Now we have to add the actual header from our handler. Let this be our handler without the header.


```
handlerWithHeader :: IO String
handlerWithHeader = pure "abc"
```

To add the header to this handler, add the final value that is being returned in a `ResponseHeader` using the `addHeader` function.


```
handlerWithHeader :: IO String
handlerWithHeader = pure $ addHeader (Proxy :: Proxy "MyHeaderName") "MyHeaderValue" "abc"
```

Note that the header name is provided again in the handler. If the header name you provide in handler differs from the one in the endpoint type, it
will be a type error.

You can include as many headers you want by including the header names in the type level list of header names in the endpoint type, and wrapping the return
value using `addHeader` function as many times as required. The order in which you wrap the return value in `ResponseHeader` must match the order in which they
are provided in the endpoint type.

For example, here is how you add two headers to the endpoint

```
"request" :> "with" :> "header" :> (GET '[PlainText] (ResponseHeader ["MyHeaderName", "MyHeaderName2"] String))
```

```
handlerWithHeader :: IO String
handlerWithHeader = pure $
  addHeader (Proxy :: Proxy "MyHeaderName") "MyHeaderValue" $
  addHeader (Proxy :: Proxy "MyHeaderName2") "MyHeaderValue2" $"abc"
```

### How to take input


#### From url - query format (?xyz=123)

Use the type `QueryParam` in the type of the url as shown below

``` 
"home" :> "post" :> QueryParam "id" Text :> (GET '[PlainText] String)
```

The above use of `QueryParam` sets the endpoint handler to recieve a `Text` value
passed via url in form `/home/post?id=asdf`. The type that is being recieved, (here `Text`)
should be an instance of [FromHttpApiData](https://hackage.haskell.org/package/http-api-data-0.4/docs/Web-HttpApiData.html#t:FromHttpApiData) typeclass in [http-api-data](http://hackage.haskell.org/package/http-api-data) package.

The handler for this endpoint must accept a `Maybe Text` as its argument. It is a `Maybe` 
because the route could be accessed without the query parameter.

```
handlerPostQueryParam :: Maybe Text -> IO String
handlerPostQueryParam (Just postId) = return $ "Post " ++ (unpack postId)
```

#### From url - segment format (/xyz/123)

Use the type `UrlParam` in the type of the url as shown below

``` 
"home" :> "post" :> UrlParam "id" Text :> (GET '[PlainText] String)
```

The above use of `UrlParam` sets the endpoint handler to recieve a `Text` value
passed via url in form `/home/post/id/my-new-post`. The type that is being recieved, (here `Text`)
should be an instance of [FromHttpApiData](https://hackage.haskell.org/package/http-api-data-0.4/docs/Web-HttpApiData.html#t:FromHttpApiData) typeclass in [http-api-data](http://hackage.haskell.org/package/http-api-data) package.

The handler for this endpoint must accept a `Text` as its argument.

```
handlerPost :: Text -> IO String
handlerPost postId = return $ "Post " ++ (unpack postId)
```
#### From url - unnamed segment format (/123)

Use the type `SegmentParam` in the type of the url as shown below

``` 
"home" :> "post" :> SegmentParam Text :> (GET '[PlainText] String)
```

The above use of `SegmentParam` sets the endpoint handler to recieve a `Text` value
passed via url in form `/home/post/my-new-post`. The type that is being recieved, (here `Text`)
should be an instance of [FromHttpwApiData](https://hackage.haskell.org/package/http-api-data-0.4/docs/Web-HttpApiData.html#t:FromHttpApiData) typeclass in [http-api-data](http://hackage.haskell.org/package/http-api-data) package.

The handler for this endpoint must accept a `Text` as its argument.

```
handlerPost :: Text -> IO String
handlerPost postId = return $ "Post " ++ (unpack postId)
```

#### From Request Body

Use the type `ReqBody` in the endpoint type as shown below.

```
"home" :> "profile" :> "resume" :> "add" :> ReqBody '[JSON] Resume :> (POST '[JSON] Resume)

```

The above use of `ReqBody` enables the endpoint to get a value of type
`Resume` in `JSON` format in the body of the POST request. The type that is being recieved, (here `Resume`)
should have a `FromJSON` instance.

The handler must be a function with an argument of type `Resume`

```
handlerAddResume :: Resume -> IO Resume
handlerAddResume r = return $ r
```

#### From Request Header

Use the type `RequestHeader` in the endpoint type as shown below.

```
"request" :> "with" :> "input" :> "header" :> RequestHeader "input-header" Text :> GET '[PlainText] String
```

The above use of `ReqBody` enables the endpoint to get a value of type
`Resume` in `JSON` format in the body of the POST request.

The handler must be a function with an argument of type `Text` (because that is what is specified by `RequestHeader "input-header" Text` in url's type).
The type that is being recieved, (here `Text`) should be an instance of [FromHttpwApiData](https://hackage.haskell.org/package/http-api-data-0.4/docs/Web-HttpApiData.html#t:FromHttpApiData) typeclass in [http-api-data](http://hackage.haskell.org/package/http-api-data) package.

```
handlerHeaderInput :: Text -> IO String
handlerHeaderInput headerInput = return $ "Header value = " ++ (unpack headerInput)
```

### How to add a new output format

Output formats are handled via two typeclasses `ContentType` and `Encodable`. They are defined as 

```
class ContentType a where
  getContentType :: Proxy a -> Maybe ByteString
  doesMatch :: Proxy a -> (Maybe ByteString) -> Bool
```

```
class Encodable format a where
  encode :: a -> Proxy format -> ByteString
```

The `ContentType`'s `getContentType` function allows you to specify the mime type for your format. The `doesMatch` function allows user to implement
custom logic while matching the `Accept` header in incoming request. Either one of these needs to be implemented for the instance to be minimal.


The `Encodable` typeclass defines how a value of type `a` can be encoded in a bytestring using the `format` format.

This is how Yaar adds support for json endpoints with `JSON` format

```
instance ContentType JSON where
  getContentType _ = Just "application/json"

instance (ToJSON a) => Encodable JSON a where
  encode v _ =  LB.toStrict $ Data.Aeson.encode $ toJSON v
```

That is about it.


### How to accept a new format via RequestBody

Yaar supports `JSON` data coming in RequestBody. What if you want to accept XML data coming in a request's body. The typeclasses involved here are `RequestDerivable` and the `ContentType` typeclass we saw earlier.
`RequestDerivable` typeclass allows the user to define how a value in a certain format can be extracted from the body of the request. It is defined as follows.

```
class RequestDerivable a where
  extract :: Request -> IO (Either Status a)
```

To add support for `XML`, first we will create a dummy type for denoting `XML`

```
data XML
```

And define `ContentType` and `RequestDerivable` instances for it. Let us assume that we want to accept a value of type `User` coming in xml format.

```
instance ContentType XML where
  getContentType _ = Just "application/xml"
```

```
instance RequestDerivable (ReqBody XML User) where
  extract req = do
    body <- lazyRequestBody req  -- this is a function from Wai package that is re-exported by Yaar
    user <- do
      -- code to decode xml and create 
      -- a value of type User from it
    pure $ Right user
    
```
### How to make my Handlers run in a different Monad

By default Yaar handlers run in `IO`. But it is possible to make the handlers run in a different monad using the `RunnableTo` typeclass.

```
class RunnableTo m1 m2 e where
  runTo :: e -> m1 a -> m2 a
```

In the signature of `runTo` function, `e` is an environment value, `m1` is the monad which we want the handlers to run in and `m2` should be the `IO` monad.
So if we want our handlers to run in `Identity` monad, here is how we can do it.

```
instance RunnableTo Identity YaarHandler () where
  runTo _ im = pure $ runIdentity im
```

In the above, we have used () for environment, because we don't need an environment value to run a value of type `Identity` to `IO`. But often you want
to use something like a `Reader` monad, that requires some kind of environment to run it.

We pass the environment to the handlers via the `serve` function. Actually the `serve` function accepts a function that takes in a `Request` and returns an environment which can be used to run the actual handler.
This can be used to implement sessions and what not.

Here is how we pass the environment creation function to the serve function.

```
app :: Application
app = serve (Proxy :: Proxy AppType) appHandlers $ (\r -> ())
```

### Automatic Documentation Generation




### Internals Overview

Before we go into Internals, I just need to say that type families are a function that takes in a type and returns another type.  

```
type family MyTypeFam a :: (*->*) ->* where
  MyTypeFam a = a Int
```  


First let us see how a Haskell type gets converted into a bunch of string paths that the router can lookup while routing. Let the following be the type of our app. It has only two endpoints

```
type TestServer
   =  "home" :> "profile" :> "bio" :> (GET '[HTML] Text)
  <|> "home" :> "profile" :> "contact" :> (GET '[HTML] Text)
```

The framework, by using some simple type family stuff, converts this type above to a type level list of type level strings. Then that type level list is converted to a value level list with plain old strings that represents every single route in the app.

The following is the type family that convert the url type, into a type level list.
```
type family ExtractUrlList a :: [[Symbol]] where
  ExtractUrlList (a <|> b) = (ExtractUrl a):(ExtractUrlList b)
  ExtractUrlList a = '[ExtractUrl a]

type family ExtractUrl (a :: k) :: [Symbol] where
  ExtractUrl ((UrlParam s a) :> b) = s : "::param::": ExtractUrl b
  ExtractUrl ((a :: Symbol) :> b) = a : ExtractUrl b
  ExtractUrl (a :> b) = ExtractUrl b
  ExtractUrl (GET a) = TypeError (EUMessage "GET")
  ExtractUrl (HEAD a) = TypeError (EUMessage "POST")
  ExtractUrl (POST a) = TypeError (EUMessage "HEAD")
  ExtractUrl (PUT a) = TypeError (EUMessage "PUT")
  ExtractUrl (DELETE a) = TypeError (EUMessage "DELETE")
  ExtractUrl (CONNECT a) = TypeError (EUMessage "CONNECT")
  ExtractUrl (OPTIONS a) = TypeError (EUMessage "OPTIONS")
  ExtractUrl (PATCH a) = TypeError (EUMessage "PATCH")
  ExtractUrl (CUSTOM s a) = TypeError (EUMessage "CUSTOM s")
  ExtractUrl (GET _ a) = '["GET"]
  ExtractUrl (HEAD _ a) = '["HEAD"]
  ExtractUrl (POST _ a) = '["POST"]
  ExtractUrl (PUT _ a) = '["PUT"]
  ExtractUrl (DELETE _ a) = '["DELETE"]
  ExtractUrl (CONNECT _ a) = '["CONNECT"]
  ExtractUrl (OPTIONS _ a) = '["OPTIONS"]
  ExtractUrl (PATCH _ a) = '["PATCH"]
  ExtractUrl (CUSTOM s _ a) = '[s]

```

The type class thing that converts a type level list of symbols to a two dimensional list of strings is as follows.

```
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
```

So now we have the routes in a plain list. Next we need to find a way to represent a bunch of handlers as a Haskell type, and have a mechanism to match it with the type that represents bunch of routes that we made earlier. That is done by the class constraints, type family applications and types of the following function

```
serve
  :: forall a b e m.
  ( ManySymbolLists (ExtractUrlList a)
  , ToHandlerStack (ToHandlers YaarHandler a)
  , ExtractType b ~ m
  , ToYaarHandlers b e
  , RunnableTo m YaarHandler e
  , Convertable (ChangeEndpoint b) (ToHandlers YaarHandler a))
  => Proxy a
  -> b
  -> e
  -> Application
serve _ h env = application $ makeRoutes $ (toSymbolLists $ (Proxy :: Proxy (ExtractUrlList a))) 
```

The `a` in the first argument is the type of all the urls that make up the app. The second argument `h` is a value that wraps all the endpoint handler functions. The type safety that make sure the handlers match the url types comes from the Haskell's type checking on these two types. The third argument `env` is an environment that will be used in case of a custom end point type, to run that type into `IO`.

The structure that wraps the handlers is like an onion, with the handler of the very first endpoint at the outer most layer (Yea, just like a list in Haskell). When a http request comes in, we look at each of our routes that is in the value level list of routes. Say we find a match at index `n`. We then unwraps `n` layers of our handler wrapper and there we will have the handler to handle this very request.

### Executing the handlers

Yaar uses the following type class to actually execute the handlers.

```
class Handler a where
  execute :: Request -> a -> IO Response
```
The type machinery in Yaar converts each of your handlers functions into functions that are instances the `Handler` class. This automagically happens without any explicit instance definitions from the user.

So we will be able to run all of these handlers without having to worry about the arguments and their types as long as we have a `Request`.

### How are the handlers converted into instances of `Handler`?

Yaar defines a type class `Convertable` to generalize conversion between
two things.

```
class Convertable a b where
  convert :: a -> b
```

So let us consider a simple url type and see how this plays out.

```
"address" :> ReqBody '[JSON] Address :> POST '[HTML] Person
```


Using some type families, Yaar converts this type into a function type.

```
ReqBody '[JSON] Address -> ResponseFormat format (IO a)
```

This is the actual handler that Yaar needs to execute this route. But since
we don't want to bother our user with the whole ReqBody stuff, we use some more
type family magic and convert it to...

```
Address -> IO Person
```

And this is what we expect from user. The conversion of this function to the function 

```
ReqBody '[JSON] Address -> ResponseFormat format (IO a)
```

Is done by the generally defined instances of `Convertable` typeclass some of the relavant ones are shown below here.

```
instance {-# OVERLAPPABLE #-} Convertable a a where
  convert = id

instance {-# OVERLAPPABLE #-} (Convertable a b) => Convertable (b -> c) (a -> c) where
  convert fn = fn.convert

instance {-# OVERLAPPING #-} (Convertable b c) => Convertable (a -> b) (a -> c) where
  convert fn = convert.fn

instance {-# OVERLAPPING #-} (Convertable c a, Convertable b d) => Convertable (a -> b) (c -> d) where
  convert fn = convert.fn.convert

instance Convertable (ReqBody s a) a where
  convert (ReqBody a) = a

instance Convertable (IO a) (ResponseFormat format (IO a)) where
  convert a = ResponseFormat $ a

```

Similar conversion is applied to the whole app type via type families enabling us to write
handler functions free of framework specific types and wrappers.
