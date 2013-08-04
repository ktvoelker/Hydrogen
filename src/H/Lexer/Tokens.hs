
module H.Lexer.Tokens
  ( keywords, ident, litInt, litFloat, litChar, litBool
  , beginString, endString, stringContent
  , interpOne, beginInterp, endInterp, beginExtraDelim, endExtraDelim
  , beginBlockComment, endBlockComment, blockCommentContent
  , beginLineComment, endLineComment, lineCommentContent
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Text

import H.Common
import H.Lexer.Types

text :: Text -> Parser Text
text xs = string (T.unpack xs) *> pure xs

keywords :: TokenParser a
keywords (sKeywords -> ks) = keepMode . fmap Keyword . choice . map (try . text) $ ks

ident :: (IdClass a) => TokenParser a
ident = keepMode . choice . map oneIdent . sIdentifiers

oneIdent :: (IdClass a) => (a, Set Char, Set Char) -> Parser (Token a)
oneIdent (cls, head, tail) =
  (Identifier cls .) . (T.pack .) . (:) <$> ool head <*> many (ool tail)
  where
    ool = oneOf . S.toList

sign :: (Num b) => LexerSpec a -> Parser (b -> b)
sign = maybe (pure id) (option id . (*> pure negate) . text) . sNegative

litInt :: TokenParser a
litInt LexerSpec{ sInts = False } = mzero
litInt spec@LexerSpec{ sInts = True } = keepMode $ do
  signFunc <- sign spec
  fmap (Literal . LitInt . signFunc . read) $ many1 digit

litFloat :: TokenParser a
litFloat LexerSpec{ sFloats = False } = mzero
litFloat spec@LexerSpec{ sFloats = True } = keepMode $ do
  mainSignFunc <- sign spec
  (intPart, fracPart) <- withIntPart <|> withoutIntPart
  exp <- optionMaybe $ do
    _ <- oneOf "eE"
    expSignFunc <- sign spec
    digs <- many1 digit
    pure $ (expSignFunc . read $ digs :: Integer)
  let intVal = mainSignFunc . read $ intPart :: Integer
  let fracVal = (read fracPart :: Integer) % (10 ^ length fracPart)
  pure . Literal . LitFloat $ (fromInteger intVal + fracVal) * (10 ^ maybe 0 id exp)
  where
    withIntPart = do
      intPart <- many1 digit
      _ <- char '.'
      fracPart <- many digit
      pure (intPart, fracPart)
    withoutIntPart = do
      _ <- char '.'
      fracPart <- many1 digit
      pure ("0", fracPart)

escapeCodes :: [Parser Char]
escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) *> pure r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r') ]

charContent :: [Char] -> Parser Char
charContent specials = (char '\\' *> escape) <|> normal
  where
    escape = foldr (<|>) (unicode <|> octal <|> anyChar) escapeCodes
    unicode = do
      _ <- char 'u' :: Parser Char
      count 4 hexDigit >>= pure . chr . read . ("0x" ++)
    octal = do
      a <- oneOf ['0' .. '3']
      [b, c] <- count 2 . oneOf $ ['0' .. '7']
      pure
        . chr
        $ (readDigit a * 8 ^ (2 :: Integer)) + (readDigit b * 8) + readDigit c
    normal = noneOf $ '\\' : specials
    readDigit = read . (: [])

litChar :: TokenParser a
litChar LexerSpec{ sStrings = StringSpec{ sCharDelim = Nothing } } = mzero
litChar LexerSpec{ sStrings = StringSpec{ sCharDelim = Just quote } } =
  keepMode
  . fmap (Literal . LitChar)
  . between q q
  . charContent
  $ [quote]
  where
    q = char quote

litBool :: TokenParser a
litBool LexerSpec{ sBools = Nothing } = mzero
litBool LexerSpec{ sBools = Just (false, true) } =
  keepMode
  . fmap (Literal . LitBool)
  $ choice
    [ text false *> pure False
    , text true  *> pure True
    ]

beginString :: TokenParser a
beginString = sStrings >>> sStringDelim >>> \case
  Nothing -> mzero
  Just quote -> char quote *> pure (BeginString, [Push LMString])

stringContent :: (IdClass a) => TokenParser a
stringContent spec = keepMode $ StringContent . T.pack <$> many (charContent specials)
  where
    ss = sStrings spec
    specials = catMaybes [sStringDelim ss, fmap fst (sInterpMany ss), sInterpOne ss]

endString :: TokenParser a
endString = sStrings >>> sStringDelim >>> \case
  Nothing -> mzero
  Just quote -> char quote *> pure (EndString, [Pop LMString])

interpOne :: TokenParser a
interpOne = sStrings >>> sInterpOne >>> \case
  Nothing -> mzero
  Just sigil -> char sigil *> pure (BeginInterp, [Push LMInterpOne])

beginInterp :: TokenParser a
beginInterp = sStrings >>> sInterpMany >>> \case
  Nothing -> mzero
  Just (delim, _) -> char delim *> pure (BeginInterp, [Push LMInterp])

endInterp :: TokenParser a
endInterp = sStrings >>> sInterpMany >>> \case
  Nothing -> mzero
  Just (_, delim) -> char delim *> pure (EndInterp, [Pop LMInterp])

beginExtraDelim :: TokenParser a
beginExtraDelim = sStrings >>> sInterpMany >>> \case
  Nothing -> mzero
  Just (delim, _) -> char delim *> pure (Keyword $ T.singleton delim, [Push LMInterp])

endExtraDelim :: TokenParser a
endExtraDelim = sStrings >>> sInterpMany >>> \case
  Nothing -> mzero
  Just (_, delim) -> char delim *> pure (Keyword $ T.singleton delim, [Pop LMInterp])

beginBlockComment :: TokenParser a
beginBlockComment = sComments >>> sBlockComment >>> \case
  Nothing -> mzero
  Just (delim, _) -> try (text delim) *> pure (BeginComment, [Push LMBlockComment])

endBlockComment :: TokenParser a
endBlockComment = sComments >>> sBlockComment >>> \case
  Nothing -> mzero
  Just (_, delim) -> try (text delim) *> pure (EndComment, [Pop LMBlockComment])

blockCommentContent :: TokenParser a
blockCommentContent = sComments >>> sBlockComment >>> \case
  Nothing -> mzero
  Just (_, delim) ->
    keepMode
    . fmap (CommentContent . T.pack . concat)
    $ many (try (lookAhead (text delim)) *> pure [] <|> (: []) <$> anyChar)

beginLineComment :: TokenParser a
beginLineComment = sComments >>> sLineComment >>> \case
  Nothing -> mzero
  Just sigil -> try (text sigil) *> pure (BeginComment, [Push LMLineComment])

lineCommentContent :: TokenParser a
lineCommentContent =
  const
  . keepMode
  . fmap (CommentContent . T.pack)
  . many
  . noneOf $ ['\n']

endLineComment :: TokenParser a
endLineComment = sComments >>> sLineComment >>> \case
  Nothing -> mzero
  Just _ -> char '\n' *> pure (EndComment, [Pop LMLineComment])

