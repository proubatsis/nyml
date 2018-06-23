module UtilSpec where

import Test.Hspec
import Util

spec :: Spec
spec = do
    describe "extractQuotedText" $ do
        it "should extract the string in quotes" $ do
            (extractQuotedText "\"hello world\"") `shouldBe` ("hello world", "")
        it "should extract a string in quotes" $ do
            (extractQuotedText "abc \"hello world\"xyz") `shouldBe` ("hello world", "abc xyz")
        it "should extract the first string in quotes" $ do
            (extractQuotedText "I said \"hello world\" and you said \"stuff\"") `shouldBe` ("hello world", "I said  and you said \"stuff\"")
        it "should respect escaped quotes" $ do
            (extractQuotedText "\"hello \\\"earth\\\" world\"") `shouldBe` ("hello \"earth\" world", "")
