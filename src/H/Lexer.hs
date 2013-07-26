
module H.Lexer
  ( Token(..)
  , Literal(..)
  , IdClass
  , LexerSpec(..)
  , StringSpec(..)
  , CommentSpec(..)
  , tokenize
  ) where

import qualified Data.Conduit.List as CL
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Text

import H.Common
import H.Common.IO

data Token a =
    Keyword T.Text
  | Identifier a T.Text
  | Literal Literal
  | InterpString [Either [(Token a, SourcePos)] T.Text]
  | StartFile FilePath
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
  { sKeywords    :: [T.Text]
  , sIdentifiers :: [(a, S.Set Char, S.Set Char)]
  , sStrings     :: StringSpec
  , sInts        :: Bool
  , sNegative    :: Maybe T.Text
  , sFloats      :: Bool
  , sBools       :: Maybe (T.Text, T.Text)
  , sComments    :: CommentSpec
  } deriving (Eq, Ord, Show)

data StringSpec =
  StringSpec
  { sStringDelim :: Maybe Char
  , sCharDelim   :: Maybe Char
  , sInterpolate :: Maybe (Char, Char)
  } deriving (Eq, Ord, Show)

data CommentSpec =
  CommentSpec
  { sLineComment  :: Maybe T.Text
  , sBlockComment :: Maybe (T.Text, T.Text, Bool)
  } deriving (Eq, Ord, Show)

-- Turn each file into a Producer of tokens, and then join them all together into
-- a single Producer of tokens, and then sink that into a list
tokenize
  :: (Applicative m, Monad m, MonadIO m, IdClass a)
  => LexerSpec a -> [FilePath] -> MT n e m [(Token a, SourcePos)]
tokenize spec = liftM concat . mapM (tokenizeFile spec)

tokenizeFile
  :: (Applicative m, Monad m, MonadIO m, IdClass a)
   => LexerSpec a -> FilePath -> MT n e m [(Token a, SourcePos)]
tokenizeFile spec name = do
  xs <- liftIO . runResourceT $ sourceFile name $= decode utf8 $$ CL.consume
  case parse (file spec) (encodeString name) . T.concat $ xs of
    Left err -> do
      report . Err ELexer Nothing Nothing . Just . show $ err
      return []
    Right ts -> return $ (StartFile name, initialPos (encodeString name)) : ts

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
  , litString spec
  , litChar (sStrings spec)
  , litBool (sBools spec)
  ]

text :: T.Text -> Parser T.Text
text xs = string (T.unpack xs) >> return xs

keywords :: [T.Text] -> Parser (Token a)
keywords = fmap Keyword . choice . map (try . text)

ident :: (IdClass a) => [(a, S.Set Char, S.Set Char)] -> Parser (Token a)
ident = choice . map oneIdent

oneIdent :: (IdClass a) => (a, S.Set Char, S.Set Char) -> Parser (Token a)
oneIdent (cls, head, tail) =
  (Identifier cls .) . (T.pack .) . (:) <$> ool head <*> many (ool tail)
  where
    ool = oneOf . S.toList

sign :: (Num a) => Maybe T.Text -> Parser (a -> a)
sign = maybe (return id) $ option id . (>> return negate) . text

litInt :: Bool -> Maybe T.Text -> Parser (Token a)
litInt False _ = mzero
litInt True xs = do
  signFunc <- sign xs
  fmap (Literal . LitInt . signFunc . read) $ many1 digit

litFloat :: Bool -> Maybe T.Text -> Parser (Token a)
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

lineComment :: Maybe T.Text -> Parser ()
lineComment Nothing = mzero
lineComment (Just begin) = do
  _ <- text begin
  _ <- many . noneOf $ "\n\r"
  void . optional . char $ '\r'
  (char '\n' >> return ()) <|> eof

blockComment :: Maybe (T.Text, T.Text, Bool) -> Parser ()
blockComment Nothing = mzero
-- TODO
blockComment (Just (_, _, _)) = mzero

escapeCodes :: [Parser Char]
escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) >> return r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r') ]

charContent :: Char -> Parser Char
charContent quote = (char '\\' >> escape) <|> normal
  where
    escape = foldr (<|>) (unicode <|> octal <|> anyChar) escapeCodes
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

litString :: (IdClass a) => LexerSpec a -> Parser (Token a)
litString LexerSpec{sStrings = StringSpec{sStringDelim = Nothing}} = mzero
litString spec@LexerSpec{sStrings = StringSpec{sStringDelim = Just quote}} =
  fmap InterpString
  . between q q
  . many
  $ oneInterp spec `eitherAlt` (T.pack <$> many (charContent quote))
  where
    q = char quote

oneInterp :: forall a. (IdClass a) => LexerSpec a -> Parser [(Token a, SourcePos)]
oneInterp LexerSpec{sStrings = StringSpec{sInterpolate = Nothing}} = mzero
oneInterp spec@LexerSpec{sStrings = StringSpec{sInterpolate = Just (li, ri)}} =
  char li *> f 1
  where
    f :: Integer -> Parser [(Token a, SourcePos)]
    f 1 = char ri *> pure [] <|> g 1
    f n = g n
    g :: Integer -> Parser [(Token a, SourcePos)]
    g n = do
      posTok@(tok, _) <- withPos $ tok spec
      (posTok :) <$> f (c n tok)
    c :: Integer -> Token a -> Integer
    c n (Keyword kw)
      | kw == T.singleton li = n + 1
      | kw == T.singleton ri = n - 1
    c n _ = n

litChar :: StringSpec -> Parser (Token a)
litChar StringSpec{sCharDelim = Nothing} = mzero
litChar StringSpec{sCharDelim = Just quote} =
  fmap (Literal . LitChar) . between q q . charContent $ quote
  where
    q = char quote

litBool :: Maybe (T.Text, T.Text) -> Parser (Token a)
litBool Nothing = mzero
litBool (Just (false, true)) =
  fmap (Literal . LitBool)
  $ choice
    [ text false >> return False
    , text true  >> return True
    ]

