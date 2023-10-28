module Lex where
import Text.Regex.Posix

whitespace = " \t\n"
numRegex = "^¯?[0-9]*\\.?[0-9]+"
strRegex = "^'([^']|'')*'"
idRegex = "^[a-zA-Z_][a-zA-Z_0-9]*"

stod :: String -> Double
stod ('¯':xs) = read $ '-' : xs
stod xs = read xs

strip :: String -> String -- convert an APL string literal to the string it represents
strip s = singleize s'    -- (remove leading/trailing quotes and replace double-' with single-')
    where s' = tail . init $ s
          singleize [] = []
          singleize ('\'':'\'':xs) = '\'' : singleize xs
          singleize (x:xs) = x : singleize xs

data Token = NumTok Double
           | StrTok [Char]
           | IdTok [Char]
           | ChTok Char
    deriving (Show) -- TODO remove (debug)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize xs
    | elem (head xs) whitespace = tokenize $ tail xs
    | (length numMatch) > 0 = NumTok (stod numMatch) : tokenize (drop (length numMatch) xs)
    | (length strMatch) > 0 = StrTok (strip strMatch) : tokenize (drop (length strMatch) xs)
    | (length idMatch) > 0 = IdTok idMatch : tokenize (drop (length idMatch) xs)
    | otherwise = ChTok (head xs) : (tokenize (tail xs))
    where numMatch = xs =~ numRegex
          strMatch = xs =~ strRegex
          idMatch  = xs =~ idRegex
