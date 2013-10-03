
{-# LANGUAGE TemplateHaskell #-}
module H.Monad.Internal.Types
  ( module H.Monad.Internal.Errors
  , module H.Monad.Internal.Types
  ) where

import Data.Lens.Template
import qualified Data.Set as S

import H.Monad.Internal.Errors
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

class (Eq a, Ord a, Enum a, Bounded a, Show a) => StageNames a where

data Options n =
  Options
  { finalStage  :: Maybe n
  , debugStages :: S.Set n
  } deriving (Eq, Ord, Show)

data Result e =
    ErrResult (Err e)
  | DebugResult String
  | ArtifactResult String String
  deriving (Show)

makeLenses [''MTState]

