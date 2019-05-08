{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module TextUtils
    ( nl
    , nlTxt
    , dblQuote
    , showTxt
    ) where


import Data.Text (Text)
import qualified Data.Text as T

nl :: Text -> Text
nl = (<> nlTxt)

nlTxt :: Text
nlTxt = T.singleton '\n'

dblQuote :: Text -> Text
dblQuote txt = let a = T.singleton '"' in a <> txt <> a

showTxt :: (Show a) => a -> Text
showTxt = T.pack . show
