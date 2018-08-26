# Yaar

*** This is an experimental type level framework for bulding type safe web applications. Do not use it in production. ***

DISCLAIMER: This originally started as an exercise in type level programming,
one where I tried to implement something like the type level api provided by
the Haskell's 'Servant' framework. Implementing this was mostly a
matter of throwing random stuff at the wall and seeing what sticks.

Right now, it can do the following.

1. Can define endpoints with content type specification, multiple response formats and response type along with one or more headers in the type.
2. Can add custom data types that can be derived from a request, that handlers can recieve as arguments.
3. Can add custom formats that can be matched on a request's headers.
2. Can recieve parameters in url as well 'param/value' format.
4. Can make the handlers run in a custom monad, while the default being in `IO`.

### Getting started - The hello world app

Here we are going to create a web app with just one endpoint. 

###
```
  module Main where
  
  import Yaar
  import Data.Proxy
  import Network.Wai.Handler.Warp (run)
  
  type Server =  "hello" :> "world" :> (Get '[PlainText] String)
  
  indexHandler :: IO String
  indexHandler = return "Hello World"
  
  app = serve (Proxy :: Proxy Server) indexHandler ()
  main = run 4000 app
```

Since our app only have a single endpoint, the type of the whole server is the same as the type of the endpoint.

Then we are defining a Haskell function that will handle this endpoint. By default Yaar handlers run in IO.
Since our endpoint returns `(Get ['PlainText] String)`, we need a function of type `IO String` to handle this
endpoint.

After that, we create the `serve` function from Yaar to create a Wai Application and in the very next line we
run that Application using `run` function from Wai library.
