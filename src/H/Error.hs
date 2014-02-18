
module H.Error where

import Text.Parsec.Applicative.Types

import H.Import

data ErrType e =
  EUnknown | EInternal | ELexer | EParser | EOutput | ENotFound | ECustom e
  deriving (Eq, Ord, Show)

data Err e =
  Err
  { errType      :: ErrType e
  , errSourcePos :: Maybe SourcePos
  , errName      :: Maybe String
  , errMore      :: Maybe String
  } deriving (Show)

