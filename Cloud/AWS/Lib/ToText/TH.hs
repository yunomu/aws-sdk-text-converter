{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.Lib.ToText.TH
    ( deriveToText
    ) where

import Control.Applicative ((<$>))
import Language.Haskell.TH

import Cloud.AWS.Lib.ToText.Class (ToText(..))

deriveToText :: String -> [String] -> DecsQ
deriveToText name value = do
    let dname = mkName name
    (TyConI (DataD _ _ _ cs _)) <- reify dname
    let clauses = map
            (\(npq, str) -> clause [npq] (normalB $ stringE str) [])
            (zip (map (\(NormalC n _) -> conP n []) cs) value)
    let fun = funD 'toText clauses
    let typ = appT (conT ''ToText) (conT dname)
    (:[]) <$> instanceD (return []) typ [fun]
