module Main where

import Prelude hiding (elem)
import HTML


main :: IO ()
main = do
  putStrLn $ render $ p [] $ do
    elem $ img [ src `attribute` "dog.jpg" ]
    text "A dog"
