module Exercises where

import Text.Parser.Combinators (eof)
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'
-- read a single character '1', then die
one' = one >> stop
oneEof = char '1' >> eof

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'
oneTwoEof = char '1' >> char '2' >> eof
-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"


pNL s = putStrLn ('\n' : s)
main = do
  -- Using testParse wont compile for these two as
  -- (char '1' >> eof) :: eof :: Parsing m => m ()
  pNL "oneEof:"
  print $ parseString oneEof mempty "123"
  pNL "oneTwoEof:"
  print $ parseString oneTwoEof mempty "123"
