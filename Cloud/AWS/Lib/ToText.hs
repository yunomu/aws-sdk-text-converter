{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.Lib.ToText
    ( ToText (toText)
    , deriveToText
    ) where

import Cloud.AWS.Lib.ToText.Class (ToText(toText))
import Cloud.AWS.Lib.ToText.TH (deriveToText)
