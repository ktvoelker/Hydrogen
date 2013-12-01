
{-# LANGUAGE TemplateHaskell #-}
module H.Monad.Internal.Types
  ( module H.Monad.Internal.Errors
  , module H.Monad.Internal.Types
  ) where

import Data.Lens.Template
import qualified Data.Set as S

import H.Monad.Internal.Errors
import H.Import

newtype PrimId = PrimId { primName :: Text } deriving (Eq, Ord, Show)

primId :: Text -> PrimId
primId = PrimId

data Unique = Prim PrimId | Unique Integer Text deriving (Show)

primUnique :: PrimId -> Unique
primUnique = Prim

instance Eq Unique where
  (==) (Prim xs) (Prim ys) = xs == ys
  (==) (Unique m _) (Unique n _) = m == n
  (==) (Prim _) (Unique _ _) = False
  (==) (Unique _ _) (Prim _) = False

instance Ord Unique where
  compare (Prim xs) (Prim ys) = compare xs ys
  compare (Unique m _) (Unique n _) = compare m n
  compare (Prim _) (Unique _ _) = LT
  compare (Unique _ _) (Prim _) = GT

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

