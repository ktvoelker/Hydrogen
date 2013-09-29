
{-# LANGUAGE TemplateHaskell #-}
module H.Monad.Types where

import Data.Lens.Template

import H.Import

data Unique =
  Unique
  { uUnique :: Integer
  , uName   :: Text
  } deriving (Show)

instance Eq Unique where
  (==) = (==) `on` uUnique

instance Ord Unique where
  compare = comparing uUnique

data MTState =
  MTState
  { _mtNextUnique :: Integer
  } deriving (Show)

emptyMTState = MTState 0

makeLenses [''MTState]

