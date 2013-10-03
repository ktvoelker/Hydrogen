
module H.Parser where

import qualified Data.Map as M
import Text.Parsec.Applicative hiding (Parser, parse)
import Text.Parsec.Applicative.Pos
import qualified Text.Parsec.Applicative as P

import H.Common
import H.Lexer (TokenType(..), TokenData(..), IdClass(), Tokens)

type Parser s a = P.Parser s (TokenType a) TokenData

instance SourcePosEffect (Parser s a) where
  getSourcePos = getPosition

parse
  :: (Eq a, Applicative m, Monad m)
  => Parser s a b
  -> FileMap (Tokens a)
  -> MT n e m (FileMap b)
parse = (sequence .) . (M.mapWithKey . parseFile)

parseFile
  :: (Eq a, Applicative m, Monad m)
  => Parser s a b
  -> FilePath
  -> Tokens a
  -> MT n e m b
parseFile file _ xs = case P.parse file . map fst $ xs of
  Left err -> fatal . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

delimit :: (IdClass a) => Text -> Text -> Parser s a b -> Parser s a b
delimit ld rd = between (kw ld) (kw rd)

kw :: (IdClass a) => Text -> Parser s a ()
kw = tok . Keyword

litInt :: (IdClass a) => Parser s a Integer
litInt = intData . snd <$> token LitInt

litFloat :: (IdClass a) => Parser s a Rational
litFloat = floatData . snd <$> token LitFloat

litChar :: (IdClass a) => Parser s a Char
litChar = charData . snd <$> token LitChar

litBool :: (IdClass a) => Parser s a Bool
litBool = boolData . snd <$> token LitBool

identifier :: (IdClass a) => a -> Parser s a Text
identifier = (textData . snd <$>) . token . Identifier

anyIdentifier :: (IdClass a) => Parser s a (a, Text)
anyIdentifier = fmap f . choice $ map (token . Identifier) [minBound .. maxBound]
  where
    f (Identifier cls, TextData name) = (cls, name)
    f _ = undefined

tok :: (Eq a) => TokenType a -> Parser s a ()
tok = (f <$>) . token
  where
    f (_, NoData) = ()
    f _ = undefined

beginString :: (IdClass a) => Parser s a ()
beginString = tok BeginString

endString :: (IdClass a) => Parser s a ()
endString = tok EndString

stringContent :: (IdClass a) => Parser s a Text
stringContent = textData . snd <$> token StringContent

beginInterp :: (IdClass a) => Parser s a ()
beginInterp = tok BeginInterp

endInterp :: (IdClass a) => Parser s a ()
endInterp = tok EndInterp

beginComment :: (IdClass a) => Parser s a ()
beginComment = tok BeginComment

commentContent :: (IdClass a) => Parser s a Text
commentContent = textData . snd <$> token CommentContent

endComment :: (IdClass a) => Parser s a ()
endComment = tok EndComment

