module ParserSpec where

import Test.Hspec
import Tokenizer
import Parser

spec :: Spec
spec = do
    describe "parseTokens" $ do
        it "should parse a string" $ do
            (parse (Line 1 [StringToken "hello"]))
            `shouldBe`
            (NymlString "hello")
        it "should parse an int" $ do
            (parse (Line 1 [IntToken 5]))
            `shouldBe`
            (NymlInt 5)
        it "should parse a float" $ do
            (parse (Line 1 [FloatToken 7.0]))
            `shouldBe`
            (NymlFloat 7.0)
        it "should parse a true bool" $ do
            (parse (Line 1 [BoolToken True]))
            `shouldBe`
            (NymlBool True)
        it "should parse a false bool" $ do
            (parse (Line 1 [BoolToken False]))
            `shouldBe`
            (NymlBool False)
        it "should parse an object" $ do
            (parse (Children 0
                [ Line 1 [StringToken "name", ColonToken, StringToken "Bob"]
                , Line 2 [StringToken "age", ColonToken, IntToken 10]]))
            `shouldBe`
            (NymlObject
                [ ("name", NymlString "Bob")
                , ("age", NymlInt 10)])
        it "should parse nested objects" $ do
            (parse (Children 0
                    [ Line 1 [StringToken "person", ColonToken]
                    , Children 2
                        [ Line 1 [StringToken "name", ColonToken, StringToken "Bob"]
                        , Line 2 [StringToken "age", ColonToken, IntToken 10]]]))
            `shouldBe`
            (NymlObject
                [("person", (NymlObject
                    [ ("name", NymlString "Bob")
                    , ("age", NymlInt 10)]))])
