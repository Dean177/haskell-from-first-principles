{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.Trifecta

newtype Comment = Comment String deriving (Eq, Show)
newtype Header = Header String deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Properties = Map Name Value

data Section = Section Header Properties deriving (Eq, Show)
newtype IniConfig = IniConfig (Map Header Section) deriving (Eq, Show)

skipEols :: Parser ()
skipEols = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

commentParser :: Parser Comment
commentParser =
  string "; " *> (Comment <$> some (noneOf "\n")) <* string "\n"

commentSkipper :: Parser ()
commentSkipper = skipMany commentParser

headerParser :: Parser Header
headerParser = char '[' *> (Header <$> some letter) <* char ']'

propertyParser :: Parser (Name, Value)
propertyParser = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEols
  return (name, value)

sectionParser :: Parser Section
sectionParser = do
  skipWhitespace
  commentSkipper
  header <- headerParser
  skipEols
  properties <- some propertyParser
  return $ Section header (Map.fromList properties)

iniConfigParser :: Parser IniConfig
iniConfigParser = do
  sections <- some sectionParser
  let sectionsWithHeader = fmap (\section@(Section header _) -> (header, section))
  return $ IniConfig $ Map.fromList (sectionsWithHeader sections)

headerEx :: ByteString
headerEx = "[blah]"

propertyEx :: ByteString
propertyEx = "woot=1"

propertyEx' :: ByteString
propertyEx' = "name=value"

commentEx :: ByteString
commentEx = "; some comment"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = "; ;comment \n[section]\n host=wikipedia.org\n alias=claw"

sectionEx'' :: ByteString
sectionEx'' = "; comment\n[section]\nhost=wikipedia.org\nalias=claw\n[whatisit]\nred=intoothandclaw"

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Property Parsing" $
    it "can parse properties" $ do
      let m = parseByteString propertyParser mempty propertyEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
    let m = parseByteString headerParser mempty headerEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "Can skip a comment before a header" $ do
    let p = commentSkipper >> headerParser
        i = "; woot\n[blah]"
        m = parseByteString p mempty i
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "Can parse a simple section" $ do
      let m = parseByteString sectionParser mempty sectionEx
          r' = maybeSuccess m
          states = Map.fromList [("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString iniConfigParser mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = Map.fromList [("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = Map.fromList [("red", "intoothandclaw")]
          expected' = Just (IniConfig (Map.fromList
            [
            (Header "section", Section (Header "section") sectionValues),
            (Header "whatisit", Section (Header "whatisit") whatisitValues)
            ]))
      print m
      r' `shouldBe` expected'
