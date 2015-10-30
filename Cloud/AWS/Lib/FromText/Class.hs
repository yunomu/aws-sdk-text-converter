{-# LANGUAGE FlexibleInstances #-}

module Cloud.AWS.Lib.FromText.Class
    where

import Control.Applicative ((<$>))
import Control.Monad
import Data.IP (IPv4, AddrRange)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Parse as TP
import Safe

class FromText a where
    fromText :: Monad m => Text -> m a

    fromText' :: Monad m => Text -> m a
    fromText' name
        = maybe (failText name) return
        . fromText
        $ name

    fromNamedText :: Monad m => Text -> Maybe Text -> m a
    fromNamedText name
        = maybe
            (failText $ T.pack "no text name=" <> name)
            fromText'

failText :: Monad m => Text -> m a
failText msg = fail $ "FromText error: " <> T.unpack msg

instance FromText a => FromText (Maybe a) where
    fromText' = return . join . fromText
    fromNamedText _name Nothing  = return Nothing
    fromNamedText _name (Just t) = return $ fromText t
    fromText = return . fromText

instance FromText Int where
    fromText = fromTextToRead

instance FromText Integer where
    fromText = fromTextToRead

instance FromText Double where
    fromText = fromTextToRead

instance FromText IPv4 where
    fromText = fromTextToRead

instance FromText (AddrRange IPv4) where
    fromText = fromTextToRead

fromTextToRead :: (Monad m, Read a) => Text -> m a
fromTextToRead = readM . T.unpack

readM :: (Monad m, Read a) => String -> m a
readM a = maybe (fail $ "read failue: " <> a) return $ readMay a

instance FromText Text where
    fromText t
        | t == ""   = failText "Text"
        | otherwise = return t

instance FromText Bool where
    fromText "true"  = return True
    fromText "false" = return False
    fromText _       = failText "Bool"

instance FromText UTCTime where
    fromText t
        = maybe
            (fail "UTCTime")
            (return . Time.localTimeToUTC Time.utc)
        $ fst <$> (TP.strptime fmt $ T.unpack t)
      where
        fmt = "%FT%T"

instance FromText () where
    fromText _ = return ()
    fromNamedText _ = maybe (return ()) fromText
