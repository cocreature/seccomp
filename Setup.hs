module Main (main) where

import Distribution.Extra.Doctest ( defaultMainWithDoctests )
main :: IO ()
main = do
  defaultMainWithDoctests "doctests"
