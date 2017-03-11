-- 1. No, no instance of Show was provided
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. No, x was compledely polymorphic, and had no way of being ocmpared to Woot.
data Mood = Blah | Woot deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x =
  if (x == Woot)
    then Blah
    else x

-- 3. If you were able to get settleDown to typecheck:
  -- a) What values are acceptable inputs to that function?
  -- Moods
  -- b) What will happen if you try to run settleDown 9? Why?
  -- A compile error, it doesn't match the signature
  -- c) What will happen if you try to run Blah > Woot? Why?
  -- Compile error, Mood has no instance of Ord

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


