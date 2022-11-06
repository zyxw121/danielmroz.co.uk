import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Time 
import Data.Time.Format.ISO8601
import Data.Time.Calendar.OrdinalDate

stringToDate = iso8601ParseM


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Date Parsing" $ do
    it "succeeds for good strings" $ do
      stringToDate "2022-01-01" `shouldBe` (Just $ fromOrdinalDate 2022 1 :: Maybe Day)
    it "fails for bad strings" $ do
      stringToDate "2022-1-1" `shouldBe` (Nothing :: Maybe Day)
    
