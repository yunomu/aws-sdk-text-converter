{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative ((<$>), (<*>))
import Data.IP
import Data.Text
import Data.Time
import Test.Hspec
import Test.HUnit
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Cloud.AWS.Lib.FromText
import Cloud.AWS.Lib.ToText

prop_class :: (Eq a, FromText a, ToText a) => a -> Bool
prop_class a = fromText (toText a) == Just a

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> choose (0, 60*60*24-1)

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary IPv4 where
    arbitrary = toIPv4 <$> vectorOf 4 (choose (0,255))

instance Arbitrary (AddrRange IPv4) where
    arbitrary = makeAddrRange <$> arbitrary <*> choose (0, 32)

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

prop_mclass :: (Eq a, FromText a, ToText a) => a -> Bool
prop_mclass a = fromText (toText a) == Just (Just a)

testNothing :: IO ()
testNothing = (fromText "" :: Maybe Int) @=? Nothing

testConvertNothingToUnit :: IO ()
testConvertNothingToUnit = fromNamedText "unit" Nothing @=? Just ()

data A = B | C deriving (Eq, Show)

deriveToText "A" ["ab", "ac"]
deriveFromText "A" ["ab", "ac"]

instance Arbitrary A where
    arbitrary = elements [B, C]

main :: IO ()
main = hspec $ do
    describe "Cloud.AWS.Lib.{FromText,ToText}" $ do
        prop "Int" (prop_class :: Int -> Bool)
        prop "Integer" (prop_class :: Integer -> Bool)
        prop "Double" (prop_class :: Double -> Bool)
        prop "Bool" (prop_class :: Bool -> Bool)
        prop "UTCTime" (prop_class :: UTCTime -> Bool)
        prop "IPv4" (prop_class :: IPv4 -> Bool)
        prop "AddrRange IPv4" (prop_class :: AddrRange IPv4 -> Bool)
        prop "Maybe Int" (prop_mclass :: Int -> Bool)
        it "convert Nothing" testNothing
        prop "deriveFromText/deriveToText" (prop_class :: A -> Bool)
        prop "Unit" (\t -> fromText t == Just ())
        it "convert Nothing to Unit" testConvertNothingToUnit
