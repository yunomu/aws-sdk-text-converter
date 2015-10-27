{-# LANGUAGE FlexibleInstances #-}

module Cloud.AWS.Lib.ToText.Class
    ( ToText (toText)
    ) where

import Data.ByteString (ByteString)
import Data.IP (IPv4, AddrRange)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.Locale.Compat (defaultTimeLocale)

class ToText a where
    toText :: a -> Text
    toMaybeText :: a -> Maybe Text
    toMaybeText = Just . toText

instance ToText Text where
    toText t = t

instance ToText ByteString where
    toText = T.decodeUtf8

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

instance ToText UTCTime where
    toText
        = T.pack
        . Time.formatTime defaultTimeLocale "%FT%T"

instance ToText Int where
    toText = toTextFromShow

instance ToText Integer where
    toText = toTextFromShow

instance ToText Double where
    toText = toTextFromShow

instance ToText IPv4 where
    toText = toTextFromShow

instance ToText (AddrRange IPv4) where
    toText = toTextFromShow

toTextFromShow :: Show a => a -> Text
toTextFromShow = T.pack . show
