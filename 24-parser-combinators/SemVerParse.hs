module SemVerParse where

import Data.Monoid
import Control.Applicative ((<|>), liftA2, pure)
import Text.Trifecta

data NumberOrString = NosNumber Integer | NosString String deriving (Eq, Ord, Show)

data SemanticVersion = SemanticVersion {
  major :: Integer,
  minor :: Integer,
  patch :: Integer,
  release :: [NumberOrString],
  metaData :: [NumberOrString]
} deriving (Eq, Show)

instance Ord SemanticVersion where
  (SemanticVersion major minor patch release metaData) `compare` (SemanticVersion major' minor' patch' release' metaData') =
    compare major major' <>
    compare minor minor' <>
    compare patch patch' <>
    compare release release' <>
    compare metaData metaData'

dotSeparatedAlphaNumDash :: Parser [NumberOrString]
dotSeparatedAlphaNumDash =
  ((NosNumber <$> try decimal) <|>
    (NosString <$> some (alphaNum <|> char '-')))
   `sepBy` char '.'

releaseParser :: Parser [NumberOrString]
releaseParser = char '-' *> dotSeparatedAlphaNumDash

metaDataParser :: Parser [NumberOrString]
metaDataParser = char '+' *> dotSeparatedAlphaNumDash

-- This currently fails http://semver.org/#spec-item-2
-- by allowing leading zeros
semVerParser :: Parser SemanticVersion
semVerParser = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  releaseInfo <- releaseParser <|> pure []
  metaData <- metaDataParser <|> pure []
  return $ SemanticVersion major minor patch releaseInfo metaData

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

charToDigit :: Char -> Integer
charToDigit chr = case chr of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9

parseBase10Integer :: Parser Integer
parseBase10Integer = do
  negationSymbols <- many $ char '-'
  digitChars <- some parseDigit
  let digits = fmap charToDigit digitChars
  let accumulatedNumber = foldl (\acc curr -> (acc * 10) + curr) 0 digits
  let isPositive = negationSymbols == []
  return $ if isPositive then accumulatedNumber else negate accumulatedNumber

main :: IO ()
main = do
  print $ compare (NosNumber 123) (NosNumber 123) == EQ
  print $ compare (NosNumber 123) (NosString "123") == LT
  print $ compare (NosString "123") (NosNumber 123) == GT
  print $ compare (NosString "123") (NosString "123") == EQ
  print $ compare (NosString "alpha1") (NosString "alpha2") == LT
  print $ liftA2 (==) (parseString semVerParser mempty "2.1.1") (Success $ SemanticVersion 2 1 1 [] [])
  print $ liftA2 (==) (parseString semVerParser mempty "1.0.0-x.7.z.92") (Success $ SemanticVersion 1 0 0 [NosString "x", NosNumber 7, NosString "z", NosNumber 92] [])
  print $ (SemanticVersion 2 1 1 [] [] > SemanticVersion 2 1 0 [] []) == True
