
{-# LANGUAGE TemplateHaskell #-}
module H.Annotation.Internal where

import Data.Lens.Template
import Text.Parsec.Pos (SourcePos())

import H.Import

data Ann =
  Ann
  { _annSourcePos :: Maybe SourcePos
  } deriving (Eq, Ord, Show)

emptyAnn :: Ann
emptyAnn = Ann Nothing

makeLenses [''Ann]

