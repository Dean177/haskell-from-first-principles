-- length :: [a] -> Integer

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

fixedDiv = div 6 (length awesome)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome word = word == reverse word

myAbs :: Integer -> Integer
myAbs int =
  if (int < 0)
  then (-int)
  else int

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f first second = ((snd first, snd second), (fst first, fst second))

x = (+)
messedAdder :: [a] -> Int
messedAdder xs = x w 1
  where w = length xs

iden x = x

pluckHeadFirst a = x
  where x : xs = a

sliceTupleFirst (a, b) = a

