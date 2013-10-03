
module H.Lexer.Tokens
  ( keywords, ident, litInt, litFloat, litChar, litBool
  , beginString, endString, stringContent
  , beginInterp, endInterp, beginExtraDelim, endExtraDelim
  , beginBlockComment, endBlockComment, blockCommentContent
  , beginLineComment, endLineComment, lineCommentContent
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Text.Regex.Applicative

import H.Common
import H.Lexer.Types

text :: Text -> Parser Text
text xs = string (T.unpack xs) *> pure xs

keywords :: TokenParser a
keywords =
  keepMode
  . fmap ((,NoData) . Keyword)
  . choice
  . map text
  . sKeywords

ident :: (IdClass a) => TokenParser a
ident = keepMode . choice . map oneIdent . sIdentifiers

oneIdent :: (IdClass a) => (a, Set Char, Set Char) -> Parser (Token a)
oneIdent (cls, head, tail) =
  ((Identifier cls,) .)
  . (TextData .)
  . (T.pack .)
  . (:) <$> ool head <*> many (ool tail)
  where
    ool = oneOf . S.toList

sign :: (Num b) => LexerSpec a -> Parser (b -> b)
sign = maybe (pure id) (option id . (*> pure negate) . text) . sNegative

litInt :: TokenParser a
litInt LexerSpec{ sInts = False } = empty
litInt spec@LexerSpec{ sInts = True } = keepMode $ f <$> sign spec <*> many1 digit
  where
    f signFunc = (LitInt,) . IntData . signFunc . read

litFloat :: TokenParser a
litFloat LexerSpec{ sFloats = False } = empty
litFloat spec@LexerSpec{ sFloats = True } =
  keepMode $ f <$> sign spec <*> (withIntPart <|> withoutIntPart) <*> g'
  where
    f mainSignFunc (intPart, fracPart) exp =
      (LitFloat,)
      . FloatData
      $ (fromInteger intVal + fracVal) * (10 ^ maybe 0 id exp)
      where
        intVal = mainSignFunc . read $ intPart :: Integer
        fracVal = (read fracPart :: Integer) % (10 ^ length fracPart)
    g expSignFunc digs = expSignFunc . read $ digs :: Integer
    g' = optionMaybe $ g <$> (oneOf "eE" *> sign spec) <*> many1 digit
    withIntPart = (,) <$> many1 digit <*> (char '.' *> many digit)
    withoutIntPart = ("0",) <$> (char '.' *> many1 digit)

escapeCodes :: [Parser Char]
escapeCodes =
  map (\(e, r) -> (char e :: Parser Char) *> pure r)
    [ ('b', '\b'), ('t', '\t'), ('n', '\n'), ('f', '\f'), ('r', '\r') ]

charContent :: [Char] -> Parser Char
charContent specials = (char '\\' *> escape) <|> normal
  where
    escape = foldr (<|>) (unicode <|> octal <|> anyChar) escapeCodes
    octal = f <$> oneOf ['0' .. '3'] <*> (count 2 . oneOf $ ['0' .. '7'])
    f a [b, c] =
      chr $ (readDigit a * 8 ^ (2 :: Integer)) + (readDigit b * 8) + readDigit c
    f _ _ = undefined
    unicode =
      chr . read . ("0x" ++) <$> ((char 'u' :: Parser Char) *> count 4 hexDigit)
    normal = noneOf $ '\\' : specials
    readDigit = read . (: [])

litChar :: TokenParser a
litChar LexerSpec{ sStrings = StringSpec{ sCharDelim = Nothing } } = empty
litChar LexerSpec{ sStrings = StringSpec{ sCharDelim = Just quote } } =
  keepMode
  . fmap ((LitChar,) . CharData)
  . between q q
  . charContent
  $ [quote]
  where
    q = char quote

litBool :: TokenParser a
litBool LexerSpec{ sBools = Nothing } = empty
litBool LexerSpec{ sBools = Just (false, true) } =
  keepMode
  . fmap ((LitBool,) . BoolData)
  $ choice
    [ text false *> pure False
    , text true  *> pure True
    ]

beginString :: TokenParser a
beginString = sStrings >>> sStringDelim >>> \case
  Nothing -> empty
  Just quote -> char quote *> pure ((BeginString, NoData), [Push LMString])

stringContent :: (IdClass a) => TokenParser a
stringContent spec =
  keepMode
  $ (StringContent,)
  . TextData
  . T.pack <$> many1 (charContent specials)
  where
    ss = sStrings spec
    specials = catMaybes [sStringDelim ss, fmap fst (sInterpMany ss)]

endString :: TokenParser a
endString = sStrings >>> sStringDelim >>> \case
  Nothing -> empty
  Just quote -> char quote *> pure ((EndString, NoData), [Pop LMString])

beginInterp :: TokenParser a
beginInterp = sStrings >>> sInterpMany >>> \case
  Nothing -> empty
  Just (delim, _) -> char delim *> pure ((BeginInterp, NoData), [Push LMInterp])

endInterp :: TokenParser a
endInterp = sStrings >>> sInterpMany >>> \case
  Nothing -> empty
  Just (_, delim) -> char delim *> pure ((EndInterp, NoData), [Pop LMInterp])

beginExtraDelim :: TokenParser a
beginExtraDelim = sStrings >>> sInterpMany >>> \case
  Nothing -> empty
  Just (delim, _) ->
    char delim *> pure ((Keyword $ T.singleton delim, NoData), [Push LMInterp])

endExtraDelim :: TokenParser a
endExtraDelim = sStrings >>> sInterpMany >>> \case
  Nothing -> empty
  Just (_, delim) ->
    char delim *> pure ((Keyword $ T.singleton delim, NoData), [Pop LMInterp])

beginBlockComment :: TokenParser a
beginBlockComment = sComments >>> sBlockComment >>> \case
  Nothing -> empty
  Just (delim, _) -> text delim *> pure ((BeginComment, NoData), [Push LMBlockComment])

endBlockComment :: TokenParser a
endBlockComment = sComments >>> sBlockComment >>> \case
  Nothing -> empty
  Just (_, delim) -> text delim *> pure ((EndComment, NoData), [Pop LMBlockComment])

blockCommentContent :: TokenParser a
blockCommentContent = sComments >>> sBlockComment >>> \case
  Nothing -> empty
  Just _ ->
    keepMode
    . fmap ((CommentContent,) . TextData . T.pack . concat)
    $ few ((: []) <$> anyChar)

beginLineComment :: TokenParser a
beginLineComment = sComments >>> sLineComment >>> \case
  Nothing -> empty
  Just sigil -> text sigil *> pure ((BeginComment, NoData), [Push LMLineComment])

lineCommentContent :: TokenParser a
lineCommentContent =
  const
  . keepMode
  . fmap ((CommentContent,) . TextData . T.pack)
  . many
  . noneOf $ ['\n']

endLineComment :: TokenParser a
endLineComment = sComments >>> sLineComment >>> \case
  Nothing -> empty
  Just _ -> char '\n' *> pure ((EndComment, NoData), [Pop LMLineComment])

