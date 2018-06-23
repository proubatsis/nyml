module TokenizerSpec where

import Test.Hspec
import Tokenizer

spec :: Spec
spec = do
    describe "tokenizeString" $ do
        it "should tokenize an integer" $ do
            (tokenizeString "5") `shouldBe` (IntToken 5)
        it "should tokenize a float" $ do
             (tokenizeString "5.0") `shouldBe` (FloatToken 5.0)
        it "should tokenize a true boolean" $ do
            (tokenizeString "true") `shouldBe` (BoolToken True)
        it "should tokenize a false boolean" $ do
            (tokenizeString "false") `shouldBe` (BoolToken False)
        it "should tokenize an unquoted string" $ do
            (tokenizeString "thisismystring") `shouldBe` (StringToken "thisismystring")
        it "should tokenize a quoted string" $ do
            (tokenizeString "\"this is my string\"") `shouldBe` (StringToken "this is my string")
        it "should tokenize a list entry" $ do
            (tokenizeString "-") `shouldBe` ListEntryToken
        it "should tokenize a colon" $ do
            (tokenizeString ":") `shouldBe` ColonToken

    describe "tokenizeLine" $ do
        it "should tokenize indentation" $ do
            (tokenizeLine "    ") `shouldBe` [IndentationToken 4]
        it "should tokenize an integer" $ do
            (tokenizeLine "5") `shouldBe` [IntToken 5]
        it "should tokenize a float" $ do
             (tokenizeLine "5.0") `shouldBe` [FloatToken 5.0]
        it "should tokenize a true boolean" $ do
            (tokenizeLine "true") `shouldBe` [BoolToken True]
        it "should tokenize a false boolean" $ do
            (tokenizeLine "false") `shouldBe` [BoolToken False]
        it "should tokenize an unquoted string" $ do
            (tokenizeLine "thisismystring") `shouldBe` [StringToken "thisismystring"]
        it "should tokenize a quoted string" $ do
            (tokenizeLine "\"this is my string\"") `shouldBe` [StringToken "this is my string"]
        it "should tokenize a list entry" $ do
            (tokenizeLine "-") `shouldBe` [ListEntryToken]
        it "should tokenize a colon" $ do
            (tokenizeLine ":") `shouldBe` [ColonToken]
        it "should tokenize a key-value pair" $ do
            (tokenizeLine "name: value") `shouldBe` [StringToken "name", ColonToken, StringToken "value"]
        it "should tokenize an indented key-value pair" $ do
            (tokenizeLine "    name: value") `shouldBe` [IndentationToken 4, StringToken "name", ColonToken, StringToken "value"]
        it "should tokenize an indented key-value pair with a quoted value" $ do
            (tokenizeLine "    name: \"my value\"") `shouldBe` [IndentationToken 4, StringToken "name", ColonToken, StringToken "my value"]
        it "should tokenize a list entry with a string" $ do
            (tokenizeLine "- hello") `shouldBe` [ListEntryToken, StringToken "hello"]
        it "should tokenize an indented list entry with a string" $ do
            (tokenizeLine "  - hello") `shouldBe` [IndentationToken 2, ListEntryToken, StringToken "hello"]
        it "should tokenize an indented list entry with a key-value pair" $ do
            (tokenizeLine "  - name: value") `shouldBe` [IndentationToken 2, ListEntryToken, StringToken "name", ColonToken, StringToken "value"]
        it "should handle an escaped quote" $ do
            (tokenizeLine "\"this is \\\"my\\\" string\"") `shouldBe` [StringToken "this is \"my\" string"]
    describe "tokenizeLines" $ do
        it "should tokenize a given string" $ do
            (tokenizeLines "name: value\nlist:\n- \"item 1\"\n- item2\nobj:\n  name: value")
                `shouldBe` [
                    Line 1 [StringToken "name", ColonToken, StringToken "value"]
                    , Line 2 [StringToken "list", ColonToken]
                    , Line 3 [ListEntryToken, StringToken "item 1"]
                    , Line 4 [ListEntryToken, StringToken "item2"]
                    , Line 5 [StringToken "obj", ColonToken]
                    , Line 6 [IndentationToken 2, StringToken "name", ColonToken, StringToken "value"]
                    ]
    describe "prependIndentationTokens" $ do
        it "should prepend an indentation token with a value of 0 if there is no indentation token" $ do
            (prependIndentationTokens [Line 1 [ColonToken]]) `shouldBe` [Line 1 [IndentationToken 0, ColonToken]]
        it "should prepend an indentation token if one is already present" $ do
            (prependIndentationTokens [Line 1 [IndentationToken 4, ColonToken]]) `shouldBe` [Line 1 [IndentationToken 4, ColonToken]]
        it "should prepend an indentation token to any entries that do not have one" $ do
            (prependIndentationTokens [Line 1 [IndentationToken 4, ColonToken], Line 2 [ColonToken]]) `shouldBe`
                [Line 1 [IndentationToken 4, ColonToken], Line 2 [IndentationToken 0, ColonToken]]
    describe "tokenize" $ do
        it "should tokenize key-value pairs" $ do
            (tokenize "name: me\nage: 100") `shouldBe` (Children 0
                [ Line 1 [StringToken "name", ColonToken, StringToken "me"]
                , Line 2 [StringToken "age", ColonToken, IntToken 100]])
        it "should tokenize nested key-value pairs" $ do
            (tokenize "person:\n  name: joe\n  age: 100\nname: bob") `shouldBe`
                (Children 0
                    [ Line 1 [StringToken "person", ColonToken]
                    , Children 2
                        [ Line 2 [StringToken "name", ColonToken, StringToken "joe"]
                        , Line 3 [StringToken "age", ColonToken, IntToken 100]]
                    , Line 4 [StringToken "name", ColonToken, StringToken "bob"]])

