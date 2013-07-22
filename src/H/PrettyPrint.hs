
module H.PrettyPrint
  ( module Text.PrettyPrint
  , module H.PrettyPrint
  ) where

import qualified Data.Text as T
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding
  ( text, ptext, sizedText, zeroWidthText
  , render, renderStyle, fullRender, TextDetails(..)
  )

import H.Common

text :: T.Text -> Doc
text = PP.text . T.unpack

sizedText :: Int -> T.Text -> Doc
sizedText n = PP.sizedText n . T.unpack

zeroWidthText :: T.Text -> Doc
zeroWidthText = PP.zeroWidthText . T.unpack

render :: Doc -> T.Text
render = T.pack . PP.render

renderStyle :: Style -> Doc -> T.Text
renderStyle s = T.pack . PP.renderStyle s

fullRender :: Mode -> Int -> Float -> (TextDetails -> a -> a) -> a -> Doc -> a
fullRender m ll rl f end doc = PP.fullRender m ll rl f' end doc
  where
    f' (PP.Chr c)   = f (Chr c)
    f' (PP.Str xs)  = f (Str . T.pack $ xs)
    f' (PP.PStr xs) = f (Str . T.pack $ xs)

data TextDetails = Chr !Char | Str T.Text

