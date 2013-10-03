
module H.Monad.Internal.Errors where

import Text.Parsec.Applicative.Pos

import H.Import

data ErrType e =
  EUnknown | EInternal | ELexer | EParser | EOutput | ENotFound | ECustom e
  deriving (Eq, Ord, Show)

instance (Bounded e) => Bounded (ErrType e) where
  minBound = EUnknown
  maxBound = ECustom maxBound

instance (Enum e) => Enum (ErrType e) where
  toEnum 0 = EUnknown
  toEnum 1 = EInternal
  toEnum 2 = ELexer
  toEnum 3 = EParser
  toEnum 4 = EOutput
  toEnum n = ECustom $ toEnum $ n - 5
  fromEnum EUnknown = 0
  fromEnum EInternal = 1
  fromEnum ELexer = 2
  fromEnum EParser = 3
  fromEnum EOutput = 4
  fromEnum ENotFound = 5
  fromEnum (ECustom e) = fromEnum e + 6

data Err e =
  Err
  { errType      :: ErrType e
  , errSourcePos :: Maybe SourcePos
  , errName      :: Maybe String
  , errMore      :: Maybe String
  } deriving (Show)

instance Error (Err e) where
  noMsg  = Err EUnknown Nothing Nothing Nothing
  strMsg = Err EUnknown Nothing Nothing . Just

newtype Finished n = Finished n deriving (Eq, Ord, Show)

instance Error (Either (Err e) (Finished n)) where
  noMsg  = Left noMsg
  strMsg = Left . strMsg

