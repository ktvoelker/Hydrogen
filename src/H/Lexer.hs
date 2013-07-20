
module H.Lexer
  ( Token(..)
  , Literal(..)
  , IdClass
  , LexerSpec(..)
  , StringSpec(..)
  , CommentSpec(..)
  , tokenize
  ) where

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import H.Common

data Token a =
    Keyword String
  | Identifier a String
  | Literal Literal
  deriving (Eq, Ord, Show)

data Literal =
    LitString String
  | LitChar Char
  | LitInt Integer
  | LitFloat Rational
  | LitBool Bool
  deriving (Eq, Ord, Show)

class (Eq a, Ord a, Enum a, Bounded a, Show a) => IdClass a where

data LexerSpec a =
  LexerSpec
  { sKeywords    :: [String]
  , sIdentifiers :: [(a, [Char], [Char])]
  , sStrings     :: StringSpec
  , sInts        :: Bool
  , sNegative    :: Maybe String
  , sFloats      :: Bool
  , sBools       :: Maybe (String, String)
  , sComments    :: CommentSpec
  } deriving (Eq, Ord, Show)

data StringSpec =
  StringSpec
  { sStringDelim :: Maybe Char
  , sCharDelim   :: Maybe Char
  , sInterpolate :: Maybe (String, String)
  } deriving (Eq, Ord, Show)

data CommentSpec =
  CommentSpec
  { sLineComment  :: Maybe String
  , sBlockComment :: Maybe (String, String, Bool)
  } deriving (Eq, Ord, Show)

tokenize
  :: (Applicative m, Monad m, IdClass a)
  => LexerSpec a -> String -> String -> MT n e m [(Token a, SourcePos)]
tokenize spec name xs = case parse (file spec) name xs of
  Left err -> fatal . Err ELexer Nothing Nothing . Just . show $ err
  Right ts -> return ts

withPos parser = do
  pos <- getPosition
  ret <- parser
  return (ret, pos)

file :: (IdClass a) => LexerSpec a -> Parser [(Token a, SourcePos)]
file spec = do
  sk
  xs <- many1 (withPos $ tok spec) `sepEndBy` sk
  eof
  return . concat $ xs
  where
    sk = skippable $ sComments spec

skippable :: CommentSpec -> Parser ()
skippable cs = void . optional . many1 $ comment cs <|> (space >> return ())

tok spec =
  choice
  [ keywords (sKeywords spec)
  , ident (sIdentifiers spec)
  , litInt (sInts spec) (sNegative spec)
  , litFloat (sFloats spec) (sNegative spec)
  , litString (sStrings spec)
  , litChar (sStrings spec)
  , litBool (sBools spec)
  ]

keywords :: [String] -> Parser (Token a)
keywords = fmap Keyword . choice . map (try . string)

ident :: (IdClass a) => [(a, String, String)] -> Parser (Token a)
ident = choice . map oneIdent

oneIdent :: (IdClass a) => (a, String, String) -> Parser (Token a)
oneIdent (cls, head, tail) =
  (Identifier cls .) . (:) <$> oneOf head <*> many (oneOf tail)

sign :: (Num a) => Maybe String -> Parser (a -> a)
sign = maybe (return id) $ option id . (>> return negate) . string

litInt :: Bool -> Maybe String -> Parser (Token a)
litInt False _ = mzero
litInt True xs = do
  signFunc <- sign xs
  fmap (Literal . LitInt . signFunc . read) $ many1 digit

litFloat :: Bool -> Maybe String -> Parser (Token a)
litFloat False _ = mzero
litFloat True xs = do
  mainSignFunc <- sign xs
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    _ <- oneOf "eE"
    expSignFunc <- sign xs
    digs <- many1 digit
    return $ (expSignFunc . read $ digs :: Integer)
  let intVal = mainSignFunc . read $ intPart :: Integer
  let fracVal = (read fracPart :: Integer) % (10 ^ length fracPart)
  return . Literal . LitFloat $ (fromInteger intVal + fracVal) * (10 ^ maybe 0 id exp)
  where
    withIntPart = do
      intPart <- many1 digit
      _ <- char '.'
      fracPart <- many digit
      return (intPart, fracPart)
    withoutIntPart = do
      _ <- char '.'
      fracPart <- many1 digit
      return ("0", fracPart)

comment :: CommentSpec -> Parser ()
comment CommentSpec{..} = lineComment sLineComment <|> blockComment sBlockComment

lineComment :: Maybe String -> Parser ()
lineComment Nothing = mzero
lineComment (Just begin) = do
  _ <- string begin
  _ <- many . noneOf $ "\n\r"
  void . optional . char $ '\r'
  (char '\n' >> return ()) <|> eof

blockComment :: Maybe (String, String, Bool) -> Parser ()
blockComment Nothing = mzero
-- TODO
blockComment (Just (_, _, _)) = mzero

escapeCodes :: [Parser Char]
escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) >> return r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r'), ('"', '"')
    , ('\'', '\''), ('\\', '\\')
    ]

-- TODO support interpolation
-- 1. Allow escaping of the interpolation delimiters
-- 2. Result type of Parser (Either [(Token, SourcePos)] Char)
--    a. Inside interpolation, parse arbitrary tokens until the delimiter is reached
charContent :: Maybe (String, String) -> Char -> Parser Char
charContent _ quote = (char '\\' >> escape) <|> normal
  where
    escape = foldr (<|>) (unicode <|> octal) escapeCodes
    unicode = do
      _ <- char 'u' :: Parser Char
      count 4 hexDigit >>= return . chr . read . ("0x" ++)
    octal = do
      a <- oneOf ['0' .. '3']
      [b, c] <- count 2 . oneOf $ ['0' .. '7']
      return
        . chr
        $ (readDigit a * 8 ^ (2 :: Integer)) + (readDigit b * 8) + readDigit c
    normal = noneOf [quote, '\\']
    readDigit = read . (: [])

litString :: StringSpec -> Parser (Token a)
litString StringSpec{sStringDelim = Nothing} = mzero
litString StringSpec{sStringDelim = Just quote, sInterpolate} =
  fmap (Literal . LitString)
  . between q q
  . many
  . charContent sInterpolate
  $ quote
  where
    q = char quote

litChar :: StringSpec -> Parser (Token a)
litChar StringSpec{sCharDelim = Nothing} = mzero
litChar StringSpec{sCharDelim = Just quote} =
  fmap (Literal . LitChar) . between q q . charContent Nothing $ quote
  where
    q = char quote

litBool :: Maybe (String, String) -> Parser (Token a)
litBool Nothing = mzero
litBool (Just (false, true)) =
  fmap (Literal . LitBool)
  $ choice
    [ string false >> return False
    , string true  >> return True
    ]

