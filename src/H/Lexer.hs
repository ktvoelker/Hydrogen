
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
import H.Lexer.Tokens hiding (withPos, tok)
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
file spec = do
  sk
  xs <- toks spec `sepEndBy` sk
  eof
  return . concat $ xs
  where
    sk = skippable $ sComments spec

toks :: (IdClass a) => LexerSpec a -> Parser (Tokens a)
toks spec = fmap concat . many $ ((: []) <$> withPos (tok spec)) <|> litString spec

tok :: (IdClass a) => LexerSpec a -> Parser (Token a)
tok spec =
  choice
  [ keywords (sKeywords spec)
  , ident (sIdentifiers spec)
  , litInt (sInts spec) (sNegative spec)
  , litFloat (sFloats spec) (sNegative spec)
  , litChar (sStrings spec)
  , litBool (sBools spec)
  ]

