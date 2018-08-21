# Yaar - The friendly Haskell web framework

*** This is an experimental web framework. Do not use it in production. ***


### Getting started - The hello world app

Here we are going to create a Yaar web app with just one endpoint. 

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
```

The first thing we are doing in the code above is defining a type that will represent our endpoint. 
Since our app only have a single endpoint, the type of the whole server is the same as the type of the endpoint.

Then we are defining a Haskell function that will handle this endpoint. By default Yaar handlers run in IO.
Since our endpoint returns `(Get ['PlainText] String)`, we need a function of type `IO String` to handle this
endpoint.



