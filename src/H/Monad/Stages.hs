
module H.Monad.Stages where

import qualified Data.Set as S

import H.Import

class (Eq a, Ord a, Enum a, Bounded a, Show a) => StageNames a where

data Options n =
  Options
  { finalStage  :: Maybe n
  , debugStages :: S.Set n
  } deriving (Eq, Ord, Show)

