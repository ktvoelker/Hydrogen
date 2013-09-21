
{-# LANGUAGE TemplateHaskell #-}
module H.Lexer.Types where

import Data.Lens.Template
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos, initialPos)
import Text.Regex.Applicative

import H.Common

type Parser = RE Char

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

oneOf :: [Char] -> Parser Char
oneOf = choice . map sym

noneOf :: [Char] -> Parser Char
noneOf ps = psym $ not . (`elem` ps)

between :: Parser a -> Parser b -> Parser c -> Parser b
between l p r = l *> p <* r

option :: a -> Parser a -> Parser a
option def p = p <|> pure def

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

count :: Int -> Parser a -> Parser [a]
count n = sequenceA . replicate n

digit :: Parser Char
digit = oneOf ['0' .. '9']

hexDigit :: Parser Char
hexDigit = oneOf $ ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

spaces :: Parser ()
spaces = many (psym isSpace) *> pure ()

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

char :: Char -> Parser Char
char = sym

anyChar :: Parser Char
anyChar = anySym

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
  | LMBlockComment
  | LMLineComment
  deriving (Eq, Ord, Enum, Bounded, Show)

curMode :: [LexerMode] -> LexerMode
curMode = maybe LMNormal id . listToMaybe

emptyModeStack :: [LexerMode]
emptyModeStack = []

data LexerModeAction = Pop LexerMode | Push LexerMode
  deriving (Eq, Ord, Show)

keepMode :: (Functor f) => f a -> f (a, [LexerModeAction])
keepMode = fmap (, [])

type TokenParser a = LexerSpec a -> Parser (Token a, [LexerModeAction])

data TokenizerState =
  TokenizerState
  { _tsModeStack :: [LexerMode]
  , _tsSourcePos :: SourcePos
  , _tsInput     :: String
  } deriving (Show)

emptyTokenizerState :: String -> Text -> TokenizerState
emptyTokenizerState name = TokenizerState emptyModeStack (initialPos name) . T.unpack

makeLenses [''TokenizerState]

type TokT n e m = StateT TokenizerState (MT n e m)

