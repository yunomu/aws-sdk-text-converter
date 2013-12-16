{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.Lib.FromText.TH
    where

import Control.Applicative ((<$>))
import Language.Haskell.TH

import Cloud.AWS.Lib.FromText.Class (FromText(..))

deriveFromText :: String -> [String] -> DecsQ
deriveFromText dstr strs = do
    ctrs <- map (\(NormalC name _) -> name) <$> cons
    x <- newName "x"
    let cases = caseE (varE x) (map f (zip strs ctrs) ++ [wild])
    let fun = funD 'fromText [clause [varP x] (normalB cases) []]
    (:[]) <$> instanceD ctx typ [fun]
  where
    d = mkName dstr
    cons = do
        (TyConI (DataD _ _ _ cs _)) <- reify d
        return cs
    f (s, t) = match (litP $ stringL s) (normalB $ [|return $(conE t)|]) []
    wild = match wildP (normalB [|fail dstr|]) []
    typ = appT (conT ''FromText) (conT d)
    ctx = return []
