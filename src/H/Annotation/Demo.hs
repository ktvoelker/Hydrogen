
{-# LANGUAGE TemplateHaskell #-}
module H.Annotation.Demo where

import H.Common

annotate [d|
  data Foo = A Int | B Int String | C { a :: Int } | Int :+ Int
    deriving (Eq, Ord, Show) |]

