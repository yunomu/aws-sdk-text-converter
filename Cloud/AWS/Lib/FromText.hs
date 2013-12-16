{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Cloud.AWS.Lib.FromText
    ( FromText
      ( fromText
      , fromNamedText
      )
    , deriveFromText
    , failText
    ) where

import Cloud.AWS.Lib.FromText.Class (FromText(..), failText)
import Cloud.AWS.Lib.FromText.TH (deriveFromText)
