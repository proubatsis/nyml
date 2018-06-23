module Tokenizer (Token(..), TokenTree(..), tokenizeString, tokenizeLine, tokenizeLines, prependIndentationTokens, tokenize) where

import Data.Maybe
import Data.Strings
import Data.List
import Data.List.Split
import Text.Regex.PCRE

import Util

data Token = StringToken String
           | IntToken Int
           | FloatToken Float
           | BoolToken Bool
           | IndentationToken Int
           | ListEntryToken
           | ColonToken
           | InvalidToken deriving (Show, Eq)


data TokenTree = Line Int [Token] -- Line number, Tokens
               | Children Int [TokenTree] deriving (Show, Eq) -- Indentation Level, SubTokens


intPattern = "^(\\d+)$"
floatPattern = "^(\\d+\\.\\d+)$"
boolPattern = "^(true|false)$"
quotedStringPattern = "^\"(.+?)\"$"
unquotedStringPattern = "^([^\\s:]+)$"


matchPattern :: String -> String -> Maybe String
matchPattern s pattern = listToMaybe $ getAllTextMatches (s =~ pattern :: AllTextMatches [] String)


tokenizeString :: String -> Token
tokenizeString s
    | s == "-" = ListEntryToken
    | s == ":" = ColonToken
    | isJust intMatch = IntToken ((read $ fromJust intMatch) :: Int)
    | isJust floatMatch = FloatToken ((read $ fromJust floatMatch) :: Float)
    | isJust boolMatch = BoolToken $ (fromJust boolMatch) == "true"
    | (take 1 s, drop ((length s) - 1) s) == ("\"", "\"") = StringToken $ takeWhile (\c -> c /= '"') $ drop 1 s
    | isJust unqotedStringMatch = StringToken s
    | otherwise = InvalidToken
    where
        intMatch = matchPattern s intPattern
        floatMatch = matchPattern s floatPattern
        boolMatch = matchPattern s boolPattern
        quotedStringMatch = matchPattern s quotedStringPattern
        unqotedStringMatch = matchPattern s unquotedStringPattern


data StringType = RegularString
                | QuotedString


tokenizeLine :: String -> [Token]
tokenizeLine s
    | strStartsWith s " " =
        [IndentationToken $ length $ takeWhile (\c -> c == ' ') s] ++ (tokenizeLine $ dropWhile (\c -> c == ' ') s)
    | strStartsWith s ":" =
        [ColonToken] ++ (tokenizeLine $ strTrim $ drop 1 s)
    | strStartsWith s "\"" =
        case extractQuotedText s of (extracted, removed) -> [StringToken extracted] ++ (tokenizeLine $ strTrim removed)
    | length s > 0 =
        [tokenizeString $ takeWhile tokenPredicate s] ++ (tokenizeLine $ strTrim $ dropWhile tokenPredicate s)
    | otherwise = []

tokenPredicate :: Char -> Bool
tokenPredicate c = c /= '"' && c /= ':' && c /= ' '

tokenizeLines :: String -> [TokenTree]
tokenizeLines s =
    let
        sLines = lines s
    in
        map (\(num, line) -> Line num $ tokenizeLine line) $ zip [1 .. length sLines] sLines

prependIndentationTokens :: [TokenTree] -> [TokenTree]
prependIndentationTokens = map (\(Line n t) -> case (head t) of
    IndentationToken _ -> Line n t
    _ -> Line n $ [IndentationToken 0] ++ t)

tokenizeFromLines :: [TokenTree] -> TokenTree
tokenizeFromLines tLines = 
    let
        getIndent = \t -> case t of
            Line _ ((IndentationToken n):_) -> n
            (Children n _) -> n
            _ -> 0
        initIndent = case tLines of
            t:_ -> getIndent t
            _ -> 0
        predicate = \t -> getIndent t > initIndent
        takeNested = takeWhile predicate
        dropNested = dropWhile predicate
        go = \tl acc -> case tl of
            (Line ln ((IndentationToken n):remaining)):_ | n == initIndent -> go (tail tl) (acc ++ [Line ln remaining])
            (Line _ ((IndentationToken n):_)):_ | n > initIndent -> go (dropNested tl) (acc ++ [tokenizeFromLines $ takeNested tl])
            [] -> acc
            _ -> []
    in
        Children initIndent $ go tLines []

tokenize :: String -> TokenTree
tokenize s = tokenizeFromLines $ prependIndentationTokens $ tokenizeLines s
