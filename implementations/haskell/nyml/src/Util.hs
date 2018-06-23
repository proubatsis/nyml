module Util (extractQuotedText) where


data ExtractTextState
    = ExtractInit
    | ExtractStarted


extractQuotedText :: String -> (String, String)
extractQuotedText s = go s ExtractInit
    where
        go "" _ = ("", "")
        go ('\\':'"':xs) ExtractInit = case go xs ExtractInit of (ext, rem) -> (ext, "\\\"" ++ rem)
        go ('"':xs) ExtractInit = go xs ExtractStarted
        go ('\\':'"':xs) ExtractStarted = case go xs ExtractStarted of (ext, rem) -> ("\"" ++ ext, rem)
        go ('"':xs) ExtractStarted = ("", xs)
        go (x:xs) ExtractStarted = case go xs ExtractStarted of (ext, rem) -> ([x] ++ ext, rem)
        go (x:xs) ExtractInit = case go xs ExtractInit of (ext, rem) -> (ext, [x] ++ rem)
