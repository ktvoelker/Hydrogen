
module H.Lexer (LexerSpec(..), tokenize) where

import qualified Data.Map as M
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import H.Common

data LexerSpec a =
  LexerSpec
  { sKeywords :: [(String, a)]
  , sIntegers :: Maybe (Integer -> a)
  , sFloats   :: Maybe (Rational -> a)
  , sStrings  :: Maybe (String -> a)
  , sChars    :: Maybe (Char -> a)
  , sIdents   :: [([([Char], Integer, Maybe Integer)], String -> a)]
  }

tokenize :: LexerSpec a -> String -> String -> FM [(a, SourcePos)]
tokenize spec name xs = case parse (file spec) name xs of
  Left err -> fatal . Err ELexer Nothing Nothing . Just . show $ err
  Right ts -> return ts

withPos parser = do
  pos <- getPosition
  ret <- parser
  return (ret, pos)

file :: LexerSpec a -> Parser [(a, SourcePos)]
file spec = do
  skippable
  xs <- many1 (withPos $ tok spec) `sepEndBy` skippable
  eof
  return . concat $ xs

skippable = void . optional . many1 $ comment <|> (space >> return ())

tok spec =
  choice
  [ keywords (sKeywords spec)
  , ident (sIdents spec)
  , litInt (sIntegers spec)
  , litFloat (sFloats spec)
  , litString (sStrings spec)
  , litChar (sChars spec)
  ]

keywords xs = choice . map p . map fst $ xs
  where
    m = M.fromList xs
    p kw = do
      void $ try $ string kw
      Just tok <- return $ M.lookup kw m
      return tok

ident = choice . map oneIdent

oneIdent :: ([([Char], Integer, Maybe Integer)], String -> a) -> Parser a
oneIdent (parts, maker) = sequence (map identPart parts) >>= return . maker . concat

range :: Integer -> Maybe Integer -> Parser a -> Parser [a]
range minCount maxCount parser = do
  xs <- count (fromInteger minCount) parser
  ys <- maybe (many parser) undefined maxCount
  return $ xs ++ ys

identPart :: ([Char], Integer, Maybe Integer) -> Parser String
identPart (chars, minCount, maxCount) =
  range minCount maxCount $ choice (map char chars)

litInt Nothing = mzero
litInt (Just spec) = many1 digit >>= return . spec . read

litFloat Nothing = mzero
litFloat (Just spec) = do
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    _ <- oneOf "eE"
    sign <- optionMaybe $ oneOf "+-"
    let signVal = if sign == Just '-' then -1 else 1
    digs <- many1 digit
    return $ (signVal * read digs :: Integer)
  let intVal = read intPart :: Integer
  let fracVal = (read fracPart :: Integer) % (10 ^ length fracPart)
  return . spec $ (fromInteger intVal + fracVal) * (10 ^ maybe 0 id exp)
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

-- TODO better comment syntax, with nestable block comments
comment :: Parser ()
comment = do
  _ <- string "//"
  _ <- many . noneOf $ "\n\r"
  void . optional . char $ '\r'
  (char '\n' >> return ()) <|> eof

escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) >> return r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r'), ('"', '"')
    , ('\'', '\''), ('\\', '\\')
    ]

charContent quote = (char '\\' >> escape) <|> normal
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

litString Nothing = mzero
litString (Just spec) =
  let q = char '"' in fmap spec . between q q . many . charContent $ '"'

litChar Nothing = mzero
litChar (Just spec) =
  let q = char '\'' in fmap spec . between q q . charContent $ '\''

