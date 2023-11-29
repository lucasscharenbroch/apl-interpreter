module Lex where
import Text.Regex.PCRE.Light (compile, match, Regex, utf8)
import Data.ByteString.UTF8 (fromString, toString)

makeRegex :: String -> Regex
makeRegex s = compile (fromString s) [utf8]

(=~) :: String -> Regex -> String
(=~) s r = case match r (fromString s) [] of
           (Just (bs:_)) -> toString bs
           _ -> ""

whitespace = " \t\n"
floatRegex = makeRegex "^¯?[0-9]*\\.[0-9]+"
intRegex = makeRegex "^¯?[0-9]+"
strRegex = makeRegex "^'([^']|'')*'"
idRegex = makeRegex "^[a-zA-Z_][a-zA-Z_0-9]*"
aaRegex = makeRegex "^⍺⍺"
wwRegex = makeRegex "^⍵⍵"

stod :: String -> Double
stod ('¯':xs) = read $ '-' : xs
stod ('.':xs) = read $ "0." ++ xs
stod xs = read xs

stoi :: String -> Int
stoi ('¯':xs) = read $ '-' : xs
stoi xs = read xs

strip :: String -> String -- convert an APL string literal to the string it represents
strip s = singleize s'    -- (remove leading/trailing quotes and replace double-' with single-')
    where s' = tail . init $ s
          singleize [] = []
          singleize ('\'':'\'':xs) = '\'' : singleize xs
          singleize (x:xs) = x : singleize xs

data Token = NumTok (Either Int Double)
           | StrTok [Char]
           | IdTok [Char]
           | AATok
           | WWTok
           | ChTok Char
    deriving (Show) -- TODO remove (debug)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize xs
    | elem (head xs) whitespace = tokenize $ tail xs
    | (length floatMatch) > 0 = NumTok (Right . stod $ floatMatch) : tokenize (drop (length floatMatch) xs)
    | (length intMatch) > 0 = NumTok (Left . stoi $ intMatch) : tokenize (drop (length intMatch) xs)
    | (length strMatch) > 0 = StrTok (strip strMatch) : tokenize (drop (length strMatch) xs)
    | (length idMatch) > 0 = IdTok idMatch : tokenize (drop (length idMatch) xs)
    | (length aaMatch) > 0 = AATok : tokenize (drop (length aaMatch) xs)
    | (length wwMatch) > 0 = WWTok : tokenize (drop (length wwMatch) xs)
    | otherwise = ChTok (head xs) : (tokenize (tail xs))
    where intMatch = xs =~ intRegex
          floatMatch = xs =~ floatRegex
          strMatch = xs =~ strRegex
          idMatch  = xs =~ idRegex
          aaMatch  = xs =~ aaRegex
          wwMatch  = xs =~ wwRegex
