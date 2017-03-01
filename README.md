# Free Monad Example: Building an HTML DSL

This example is taken from the book "Learn PureScript by example", chapter 14, but here we are using
Haskell's Free Monads interface to achieve the same results. It is interesting that here I have implemented
my own version of `runFreeM` so that we can interpret a Free Monad within a given monadic computation.