module Cipher where

import Data.Char

lowercaseStart = ord 'a'
lowercaseEnd = ord 'z'
characterCount = lowercaseEnd - lowercaseStart

clamp charVal = (mod (charVal - lowercaseStart) characterCount) + lowercaseStart

encodeCharacter :: Int -> Char -> Char
encodeCharacter encodeDistance character = chr . clamp $ (ord  character) + encodeDistance

decodeCharacter :: Int -> Char -> Char
decodeCharacter encodeDistance character = encodeCharacter (-encodeDistance) character


encode :: Int -> String -> String
encode distance = map (encodeCharacter distance)

decode :: Int -> String -> String
decode distance = map (decodeCharacter distance)
