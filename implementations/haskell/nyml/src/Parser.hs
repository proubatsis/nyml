module Parser (NymlTree(..), parse) where

import Tokenizer

data NymlTree
    = NymlObject [(String, NymlTree)]
    | NymlString String
    | NymlBool Bool
    | NymlFloat Float
    | NymlInt Int deriving (Eq, Show)


parseObject :: [TokenTree] -> [(String, NymlTree)]
parseObject ((Line _ [StringToken k, ColonToken, StringToken v]):remainder) = [(k, NymlString v)] ++ (parseObject remainder)
parseObject ((Line _ [StringToken k, ColonToken, IntToken v]):remainder) = [(k, NymlInt v)] ++ (parseObject remainder)
parseObject ((Line _ [StringToken k, ColonToken, FloatToken v]):remainder) = [(k, NymlFloat v)] ++ (parseObject remainder)
parseObject ((Line _ [StringToken k, ColonToken, BoolToken v]):remainder) = [(k, NymlBool v)] ++ (parseObject remainder)
parseObject ((Line _ [StringToken k, ColonToken]):(Children ind ts):remainder) = [(k, parse (Children ind ts))] ++ (parseObject remainder)
parseObject [] = []


parse :: TokenTree -> NymlTree
parse (Line _ [StringToken s]) = NymlString s
parse (Line _ [BoolToken b]) = NymlBool b
parse (Line _ [IntToken i]) = NymlInt i
parse (Line _ [FloatToken f]) = NymlFloat f
parse (Children _ ts) = NymlObject $ parseObject ts
