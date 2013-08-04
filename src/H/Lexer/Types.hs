
module H.Lexer.Types where

import Text.Parsec (SourcePos())
import Text.Parsec.Text

import H.Common

data Token a =
    Keyword Text
  | Identifier a Text
  | Literal Literal
  | BeginString 
  | StringContent Text
  | EndString
  | BeginInterp
  | EndInterp
  | BeginComment
  | CommentContent Text
  | EndComment
  deriving (Eq, Ord, Show)

data Literal =
    LitChar Char
  | LitInt Integer
  | LitFloat Rational
  | LitBool Bool
  deriving (Eq, Ord, Show)

class (Eq a, Ord a, Enum a, Bounded a, Show a) => IdClass a where

data LexerSpec a =
  LexerSpec
  { sKeywords    :: [Text]
  , sIdentifiers :: [(a, Set Char, Set Char)]
  , sStrings     :: StringSpec
  , sInts        :: Bool
  , sNegative    :: Maybe Text
  , sFloats      :: Bool
  , sBools       :: Maybe (Text, Text)
  , sComments    :: CommentSpec
  } deriving (Eq, Ord, Show)

data StringSpec =
  StringSpec
  { sStringDelim :: Maybe Char
  , sCharDelim   :: Maybe Char
  , sInterpOne   :: Maybe Char
  , sInterpMany  :: Maybe (Char, Char)
  } deriving (Eq, Ord, Show)

data CommentSpec =
  CommentSpec
  { sLineComment  :: Maybe Text
  , sBlockComment :: Maybe (Text, Text)
  } deriving (Eq, Ord, Show)

type Tokens a = [(Token a, SourcePos)]

data LexerMode =
    LMNormal
  | LMString
  | LMInterp
  | LMInterpExtraDelim
  | LMInterpOne
  | LMBlockComment
  | LMLineComment
  deriving (Eq, Ord, Enum, Bounded, Show)

data LexerModeAction = NoAction | Pop (Maybe LexerMode) | Push LexerMode
  deriving (Eq, Ord, Show)

keepMode :: (Functor f) => f a -> f (a, LexerModeAction)
keepMode = fmap (, NoAction)

type TokenParser a = LexerSpec a -> Parser (Token a, LexerModeAction)

