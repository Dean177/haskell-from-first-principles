import Data.Char

-- validButtons = "1234567890*#"
type Digit = Char

type Chars = String

-- Valid presses: 1 and up
type Presses = Int


data DaPhone = KeyMap Digit Chars DaPhone | End deriving (Show, Eq)

phone :: DaPhone
phone = 
  KeyMap '0' " " $
  KeyMap '1' "" $
  KeyMap '2' "abc" $
  KeyMap '3' "def" $
  KeyMap '4' "ghi" $
  KeyMap '5' "jkl" $
  KeyMap '6' "mno" $
  KeyMap '7' "pqrs" $
  KeyMap '8' "tuv" $
  KeyMap '9' "wxyz" $
  KeyMap '*' "^" $
  KeyMap '#' ".," $
  End

indexOf :: (Eq a) => a -> [a] -> Int
indexOf x [] = 0
indexOf x ys = go x ys 0 
  where go x (y:ys) count = if (x == y) then count else go x ys (count + 1)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps End _ = [('1', 1)]
reverseTaps (KeyMap digit characters rest) chr
  | isUpper chr = [('*', 1)] ++ reverseTaps (KeyMap digit characters rest) (toLower chr)
  | elem chr characters = [(digit, 1 + (indexOf chr characters))]
  | otherwise = reverseTaps rest chr

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phoneLayout text = concat (map tapConverter text)
  where tapConverter = reverseTaps phoneLayout

convo :: [String] 
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

taps :: [(Digit, Presses)]
taps = cellPhonesDead phone $ convo !! 0

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps keys = foldr (+) 0 $ map snd keys

cost :: DaPhone -> Char -> Presses
cost layout = fingerTaps . reverseTaps layout

coolestLtr :: [String] -> Char 
coolestLtr = undefined
  -- map tapConverter text

coolestWord :: [String] -> String
coolestWord = undefined        
