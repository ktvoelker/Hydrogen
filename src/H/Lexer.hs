
module H.Lexer
  ( lowerAlphas
  , upperAlphas
  , alphas
  , digits
  , underscore
  , Token(..)
  , Literal(..)
  , IdClass
  , LexerSpec(..)
  , StringSpec(..)
  , CommentSpec(..)
  , Tokens
  , tokenize
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Text

import H.Common
import H.Lexer.Tokens
import H.Lexer.Types

lowerAlphas, upperAlphas, alphas, digits, underscore :: Set Char

lowerAlphas = S.fromList ['a' .. 'z']

upperAlphas = S.fromList ['A' .. 'Z']

alphas = lowerAlphas <> upperAlphas

digits = S.fromList ['0' .. '9']

underscore = S.singleton '_'

-- Turn each file into a Producer of tokens, and then join them all together into
-- a single Producer of tokens, and then sink that into a list
tokenize
  :: (Applicative m, Monad m, IdClass a)
  => LexerSpec a -> FileMap Text -> MT n e m (FileMap (Tokens a))
tokenize = (sequence .) . (M.mapWithKey . tokenizeFile)

tokenizeFile
  :: (Applicative m, Monad m, IdClass a)
   => LexerSpec a -> FilePath -> Text -> MT n e m (Tokens a)
tokenizeFile spec name xs =
  case parse (file spec) (encodeString name) xs of
    Left err -> do
      report . Err ELexer Nothing Nothing . Just . show $ err
      return []
    Right ts -> return ts

withPos parser = do
  pos <- getPosition
  ret <- parser
  return (ret, pos)

file :: (IdClass a) => LexerSpec a -> Parser (Tokens a)
file spec = skippable LMNormal *> fileBody [LMNormal] spec <* eof

fileBody :: (IdClass a) => [LexerMode] -> LexerSpec a -> Parser (Tokens a)
fileBody [] _ = undefined
fileBody ms@(m : _) spec = rec <|> base
  where
    base = if m == LMNormal then eof *> return [] else mzero
    rec = do
      ((x, as), pos) <- withPos $ tok m spec
      xs <- fileBody (foldl modeAction ms as) spec
      return $ (x, pos) : xs

modeAction :: [LexerMode] -> LexerModeAction -> [LexerMode]
modeAction ms (Push m) = m : ms
modeAction [] (Pop _) = error "Empty mode stack"
modeAction (m : ms) (Pop m')
  | m == m' = ms
  | otherwise = error "Popped the wrong mode"

alts :: [TokenParser a] -> TokenParser a
alts = foldr (\a b spec -> a spec <|> b spec) (const mzero)

normalToks :: (IdClass a) => TokenParser a
normalToks =
  alts
  [ keywords
  , ident
  , litInt
  , litFloat
  , litChar
  , litBool
  , beginString
  , beginBlockComment
  , beginLineComment
  ]

tok :: (IdClass a) => LexerMode -> TokenParser a
tok LMNormal = normalToks
tok LMString = alts [stringContent, endString, beginInterp]
tok LMInterp = alts [beginExtraDelim, endInterp, normalToks]
tok LMInterpExtraDelim = alts [endExtraDelim, normalToks]
tok LMInterpOne = fmap (\(x, as) -> (x, Pop LMInterpOne : as)) . normalToks
tok LMBlockComment = alts [beginBlockComment, endBlockComment, blockCommentContent]
tok LMLineComment = alts [endLineComment, lineCommentContent]

skippable :: LexerMode -> Parser ()
skippable LMString = pure ()
skippable _ = void $ many space

