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

Please see the `test/Spec.hs` file in this repo to see a sample app.
