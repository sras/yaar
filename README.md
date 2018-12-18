# Yaar

*** This is an experimental type level framework for bulding type safe web applications. Do not use it in production. ***

DISCLAIMER: This originally started as an exercise in type level programming,
one where I tried to implement something like the type level api provided by
the Haskell's 'Servant' framework. Implementing this was mostly a
matter of throwing random stuff at the wall and seeing what sticks.

Right now, it can do the following.

1. Endpoint handlers can have arguments that can come from the url (as segments or via key=value query pairs) or the request body.
2. Each endpoint can accept the input in multiple formats.
3. Each endpoint can respond in multiple formats.

Please see the `test/Spec.hs` file in this repo to see a sample app.

### How does this work?

Say you want to create a small web app with two end points. Let us start by making this type.

```
type TestServer
   =  "home" :> "profile" :> "bio" :> (GET '[PlainText, HTML] String)
  <|> "home" :> "profile" :> "orders" :> (GET PlainText Text)
```

By using some simple type family stuff, we convert this type above to a type level
list, and then it is converted to a value level list with plain old strings that represents every single route in the app.

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

The type class thing that converts a type level list to a plain old list is as follows.

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

Disclaimer: I got this from some stackoverflow thread. 

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

The `a` in the first argument is the type of all the urls that make up the app. The second argument `h` is a value that wraps all the endpoint handler functions. The type safety that make sure the handlers match the url types comes from the Haskell's type checking on these two types. The third argument `env` is an environment that will be used in case of a custom end point type, to run that type into IO.

