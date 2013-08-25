
module H.Parser where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec hiding (parse, (<|>), many, optional)
import qualified Text.Parsec as P

import H.Common
import H.Lexer (Token(..), Literal(..), IdClass(), Tokens)

type Parser a = Parsec (Tokens a) ()

instance MonadSourcePos (Parser a) where
  getSourcePos = getPosition

parse
  :: (Applicative m, Monad m)
  => Parser a b
  -> FileMap (Tokens a)
  -> MT n e m (FileMap b)
parse = (sequence .) . (M.mapWithKey . parseFile)

parseFile
  :: (Applicative m, Monad m)
  => Parser a b
  -> FilePath
  -> Tokens a
  -> MT n e m b
parseFile file fp xs = case P.parse file (encodeString fp) xs of
  Left err -> fatal . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

tok :: (IdClass a) => String -> (Token a -> Maybe b) -> Parser a b
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: (IdClass a) => String -> (Token a -> Bool) -> Parser a (Token a)
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: (IdClass a) => Token a -> Parser a ()
tokEq t = tokWhen (show t) (== t) >> return ()

delimit :: (IdClass a) => Text -> Text -> Parser a b -> Parser a b
delimit ld rd = between (kw ld) (kw rd)

kw :: (IdClass a) => Text -> Parser a ()
kw = tokEq . Keyword

litInt :: (IdClass a) => Parser a Integer
litInt = tok "integer" $ \case
  Literal (LitInt n) -> Just n
  _ -> Nothing

litFloat :: (IdClass a) => Parser a Rational
litFloat = tok "float" $ \case
  Literal (LitFloat f) -> Just f
  _ -> Nothing

litChar :: (IdClass a) => Parser a Char
litChar = tok "character" $ \case
  Literal (LitChar c) -> Just c
  _ -> Nothing

litBool :: (IdClass a) => Parser a Bool
litBool = tok "Boolean" $ \case
  Literal (LitBool b) -> Just b
  _ -> Nothing

identifier :: (IdClass a) => a -> Parser a Text
identifier cls = tok ("identifier (" ++ show cls ++ ")") $ \case
  Identifier cls' xs | cls == cls' -> Just xs
  _ -> Nothing

anyIdentifier :: (IdClass a) => Parser a (a, Text)
anyIdentifier = tok "identifier (any)" $ \case
  Identifier cls xs -> Just (cls, xs)
  _ -> Nothing

oneIdentifier :: (IdClass a) => Text -> Parser a ()
oneIdentifier xs = tok ("`" ++ T.unpack xs ++ "'") $ \case
  Identifier _ xs' | xs == xs' -> Just ()
  _ -> Nothing

beginString :: (IdClass a) => Parser a ()
beginString = tokEq BeginString

endString :: (IdClass a) => Parser a ()
endString = tokEq EndString

stringContent :: (IdClass a) => Parser a Text
stringContent = tok "string content" $ \case
  StringContent xs -> Just xs
  _ -> Nothing

beginInterp :: (IdClass a) => Parser a ()
beginInterp = tokEq BeginInterp

endInterp :: (IdClass a) => Parser a ()
endInterp = tokEq EndInterp

beginComment :: (IdClass a) => Parser a ()
beginComment = tokEq BeginComment

commentContent :: (IdClass a) => Parser a Text
commentContent = tok "comment content" $ \case
  CommentContent xs -> Just xs
  _ -> Nothing

endComment :: (IdClass a) => Parser a ()
endComment = tokEq EndComment

