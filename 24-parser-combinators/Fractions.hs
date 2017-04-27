{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseInteger :: Parser Integer
parseInteger = do
  int <- integer
  eof
  return int

parseIntegerTest :: IO ()
parseIntegerTest = do
 print $ parseString parseInteger mempty "123"
 print $ parseString parseInteger mempty "123abc"

shouldWork = "1/2"
shouldAlsoWork = "2/1"
badFraction = "1/0"
alsoBad = "10"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseFractionTest :: IO ()
parseFractionTest = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction

oneOfEofNewlineNumberString = "\n\
            \123\n\
            \abc\n\
            \456\n\
            \def\n"

type NumberOrString = Either Integer String
parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

type FractionOrDecimal = Either Rational Integer
parseDecFrac :: Parser FractionOrDecimal
parseDecFrac =
  skipMany (oneOf "\n") >>
    -- The `try` here allows 'failed' fractions to be parsed as decimals
    (Left <$> try parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = parseIntegerTest

