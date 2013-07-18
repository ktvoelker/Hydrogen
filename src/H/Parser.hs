
module H.Parser where

import Text.Parsec hiding (parse, (<|>), many, optional)
import qualified Text.Parsec as P

import H.Common
import H.Lexer (Token(..), Literal(..), IdClass())

type ParserInput a = [(Token a, SourcePos)]

type Parser a = Parsec (ParserInput a) ()

instance MonadSourcePos (Parser a) where
  getSourcePos = getPosition

parse
  :: (Applicative m, Monad m)
  => Parser a b
  -> String
  -> ParserInput a
  -> MT n e m b
parse file name xs = case P.parse file name xs of
  Left err -> fatal . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

tok :: (IdClass a) => String -> (Token a -> Maybe b) -> Parser a b
tok msg = (<?> msg) . token (show . fst) snd . (. fst)

tokWhen :: (IdClass a) => String -> (Token a -> Bool) -> Parser a (Token a)
tokWhen msg pred = tok msg $ \t -> if pred t then Just t else Nothing

tokEq :: (IdClass a) => Token a -> Parser a ()
tokEq t = tokWhen (show t) (== t) >> return ()

delimit :: (IdClass a) => (String, String) -> Parser a b -> Parser a b
delimit ds = between (tokEq $ Delimiter False ds) (tokEq $ Delimiter True ds)

endList :: (IdClass a) => String -> Parser a b -> Parser a [b]
endList = flip sepEndBy . tokEq . Terminator

sepList :: (IdClass a) => String -> Parser a b -> Parser a [b]
sepList = flip sepBy . tokEq . Separator

kw :: (IdClass a) => String -> Parser a ()
kw = tokEq . Keyword

end :: (IdClass a) => String -> Parser a ()
end = tokEq . Terminator

sep :: (IdClass a) => String -> Parser a ()
sep = tokEq . Separator

litInt :: (IdClass a) => Parser a Integer
litInt = tok "integer" $ \case
  Literal (LitInt n) -> Just n
  _ -> Nothing

litFloat :: (IdClass a) => Parser a Rational
litFloat = tok "float" $ \case
  Literal (LitFloat f) -> Just f
  _ -> Nothing

litString :: (IdClass a) => Parser a String
litString = tok "string" $ \case
  Literal (LitString xs) -> Just xs
  _ -> Nothing

litChar :: (IdClass a) => Parser a Char
litChar = tok "character" $ \case
  Literal (LitChar c) -> Just c
  _ -> Nothing

litBool :: (IdClass a) => Parser a Bool
litBool = tok "Boolean" $ \case
  Literal (LitBool b) -> Just b
  _ -> Nothing

identifier :: (IdClass a) => a -> Parser a String
identifier cls = tok ("identifier (" ++ show cls ++ ")") $ \case
  Identifier cls' xs | cls == cls' -> Just xs
  _ -> Nothing

anyIdentifier :: (IdClass a) => Parser a (a, String)
anyIdentifier = tok "identifier (any)" $ \case
  Identifier cls xs -> Just (cls, xs)
  _ -> Nothing

