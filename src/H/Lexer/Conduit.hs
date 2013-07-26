
module H.Lexer.Conduit where

import Data.Conduit
import qualified Data.Conduit.List as CL

import H.Common

data NoLex = NoLex

instance Error NoLex where
  noMsg = NoLex

type GenericLexer i m r = Consumer i (ErrorT NoLex m) r

type Lexer m r = GenericLexer Char m r

tokenize
  :: (Monad m)
  => GenericLexer i m ()
  -> GenericLexer i m o
  -> Conduit i (ErrorT NoLex m) o
tokenize skip tok = CL.sequence $ skip >> tok

eof :: (Monad m) => GenericLexer i m ()
eof = CL.peek >>= \case
  Nothing -> return ()
  _ -> noLex

noLex :: (Monad m) => GenericLexer a m b
noLex = lift $ throwError NoLex

exactly :: (Eq i, Monad m) => i -> GenericLexer i m i
exactly x = await >>= \case
  Just i | x == i -> return i
  _ -> noLex

oneOf :: (Eq i, Monad m) => [i] -> GenericLexer i m i
oneOf xs = await >>= \case
  Just i | i `elem` xs -> return i
  _ -> noLex

noneOf :: (Eq i, Monad m) => [i] -> GenericLexer i m i
noneOf xs = await >>= \case
  Just i | not $ i `elem` xs -> return i
  _ -> noLex

