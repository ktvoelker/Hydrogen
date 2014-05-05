
module H.Pretty
  ( module Text.PrettyPrint
  , module H.Pretty
  ) where

import qualified "base" Prelude as P
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding
  ( text, ptext, sizedText, zeroWidthText
  , render, renderStyle, fullRender, TextDetails(..)
  )

import H.Prelude

text :: Text -> Doc
text = PP.text . unpack

sizedText :: Int -> Text -> Doc
sizedText n = PP.sizedText n . unpack

zeroWidthText :: Text -> Doc
zeroWidthText = PP.zeroWidthText . unpack

render :: Doc -> Text
render = pack . PP.render

renderStyle :: Style -> Doc -> Text
renderStyle s = pack . PP.renderStyle s

fullRender :: Mode -> Int -> P.Float -> (TextDetails -> a -> a) -> a -> Doc -> a
fullRender m ll rl f end doc = PP.fullRender m ll rl f' end doc
  where
    f' (PP.Chr c)   = f (Chr c)
    f' (PP.Str xs)  = f (Str . pack $ xs)
    f' (PP.PStr xs) = f (Str . pack $ xs)

data TextDetails = Chr !Char | Str Text

