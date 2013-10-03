
module H.Lexer
  ( lowerAlphas
  , upperAlphas
  , alphas
  , digits
  , underscore
  , Token
  , TokenType(..)
  , TokenData(..)
  , IdClass
  , LexerSpec(..)
  , StringSpec(..)
  , CommentSpec(..)
  , Tokens
  , tokenize
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec.Applicative.Pos
import Text.Regex.Applicative

import H.Common
import H.Lexer.Tokens
import H.Lexer.Types

lowerAlphas, upperAlphas, alphas, digits, underscore :: Set Char

lowerAlphas = S.fromList ['a' .. 'z']

upperAlphas = S.fromList ['A' .. 'Z']

alphas = lowerAlphas <> upperAlphas

digits = S.fromList ['0' .. '9']

underscore = S.singleton '_'

tokenize
  :: (Applicative m, Monad m, IdClass a)
  => LexerSpec a -> FileMap Text -> MT n e m (FileMap (Tokens a))
tokenize = (sequence .) . (M.mapWithKey . tokenizeFile)

tokenizeFile
  :: (Applicative m, Monad m, IdClass a)
   => LexerSpec a -> FilePath -> Text -> MT n e m (Tokens a)
tokenizeFile spec name xs = runStateT (file spec) i >>= \case
  (xs, ts)
    | not . null $ tsInput ^$ ts
      -> fatal . Err ELexer Nothing Nothing . Just . show $ (tsSourcePos ^$ ts, xs)
    | curMode (tsModeStack ^$ ts) /= LMNormal
      -> fatal . Err ELexer Nothing Nothing . Just . show $ (tsModeStack ^$ ts, xs)
    | otherwise
      -> return xs
  where
    i = emptyTokenizerState (encodeString name) xs

getCurMode :: (Applicative m, Monad m) => TokT n e m LexerMode
getCurMode = curMode <$> access tsModeStack

file :: (Applicative m, Monad m, IdClass a) => LexerSpec a -> TokT n e m (Tokens a)
file spec = skip >> (sequenceWhileJust . repeat) (oneToken spec)

skip :: (Applicative m, Monad m) => TokT n e m ()
skip = void $ getCurMode >>= withPos . skippable

oneToken
  :: (Applicative m, Monad m, IdClass a)
  => LexerSpec a -> TokT n e m (Maybe (Token a, SourcePos))
oneToken spec = do
  getCurMode >>= withPos . flip tok spec >>= \case
    Nothing -> return Nothing
    Just ((x, as), pos) -> do
      void $ tsModeStack %= flip (foldl modeAction) as
      skip
      return $ Just (x, pos)

withPos :: (Applicative m, Monad m) => Parser a -> TokT n e m (Maybe (a, SourcePos))
withPos p = do
  pos <- access tsSourcePos
  findLongestPrefix (withMatched p) <$> access tsInput >>= \case
    Nothing -> return Nothing
    Just ((val, matched), rest) -> do
      void $ tsInput ~= rest
      void $ tsSourcePos %= flip updatePosString matched
      return $ Just (val, pos)

modeAction :: [LexerMode] -> LexerModeAction -> [LexerMode]
modeAction ms (Push m) = m : ms
modeAction [] (Pop _) = error "Empty mode stack"
modeAction (m : ms) (Pop m')
  | m == m' = ms
  | otherwise = error "Popped the wrong mode"

alts :: [TokenParser a] -> TokenParser a
alts = foldr (\a b spec -> a spec <|> b spec) (const empty)

normalToks :: (IdClass a) => TokenParser a
normalToks =
  alts
  [ keywords
  , litBool
  , litFloat
  , litInt
  , litChar
  , ident
  , beginString
  , beginBlockComment
  , beginLineComment
  ]

tok :: (IdClass a) => LexerMode -> TokenParser a
tok LMNormal = normalToks
tok LMString = alts [endString, beginInterp, stringContent]
tok LMInterp = alts [endInterp, beginExtraDelim, normalToks]
tok LMInterpExtraDelim = alts [endExtraDelim, normalToks]
tok LMBlockComment = alts [beginBlockComment, endBlockComment, blockCommentContent]
tok LMLineComment = alts [endLineComment, lineCommentContent]

skippable :: LexerMode -> Parser ()
skippable LMString = pure ()
skippable _ = spaces

