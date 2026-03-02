module Test.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Country (Country)
import Data.Country as Country
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype ArbCountry = ArbCountry Country

instance Arbitrary ArbCountry where
  arbitrary = ArbCountry <$> elements nea
    where
    nea = case NEA.fromArray Country.allCountries of
      Just a -> a
      Nothing -> unsafeCrashWith "allCountries is empty"

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Country" do
    it "toAlpha2 produces 2-char codes" do
      quickCheck \(ArbCountry c) ->
        String.length (Country.toAlpha2 c) == 2

    it "fromAlpha2 <<< toAlpha2 = Just (property)" do
      quickCheck \(ArbCountry c) ->
        Country.fromAlpha2 (Country.toAlpha2 c) == Just c

    it "fromAlpha2 rejects garbage" do
      Country.fromAlpha2 "ZZ" `shouldEqual` Nothing
      Country.fromAlpha2 "" `shouldEqual` Nothing

    it "allCountries has 249 entries" do
      Array.length Country.allCountries `shouldEqual` 249

    it "spot check known countries" do
      Country.toAlpha2 Country.UnitedStatesOfAmerica `shouldEqual` "US"
      Country.toAlpha2 Country.Japan `shouldEqual` "JP"
      Country.fromAlpha2 "GB" `shouldEqual` Just Country.UnitedKingdomOfGreatBritainAndNorthernIreland
