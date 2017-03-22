import Data.List

id :: a -> a
id a = a
-- :kind a == * -> *

r :: a -> f a
r = undefined
-- This doesn't make any sense to me, is it supposed to be a -> f -> a?
-- :kind r == * -> * * ???

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a


replaceThe :: String -> String
replaceThe sentence = concat $ intersperse " " $ map (replace . notThe) $ words sentence
  where
    replace Nothing = "a"
    replace (Just word) = word 

isVowel :: Char -> Bool
isVowel = flip elem $ "aeiouAEIOU"

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:_) = isVowel x

ctbv [] count = count
ctbv (a:[]) count = count
ctbv (a:(b:cs)) count 
  | (a == "the") && (startsWithVowel b) = ctbv next (count + 1) 
  | otherwise = (ctbv next count)
  where next = (b:cs)


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = ctbv (words sentence) 0

countVowels :: String -> Int
countVowels = length . (filter isVowel)


newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord word = if isValid then Just (Word' word) else Nothing
  where
    isValid = constonantsCount > vowelCount
    vowelCount = countVowels word
    constonantsCount = length word - vowelCount

data Nat = Zero | Succ Nat deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i 
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just (Succ prev)
    where Just prev = integerToNat (i - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybeeFold :: b -> (a -> b) -> Maybe a -> b
mayybeeFold x _ Nothing = x
mayybeeFold _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe fb Nothing = fb
fromMaybe _ (Just val) = val

listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList Nothing = []
maybeToList (Just x) = (x:[])

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x): xs) = (x:catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where 
    go Nothing _ = Nothing
    go _ Nothing = Nothing
    go (Just x) (Just xs) = Just (x:xs)

lefts' [] = []
lefts' ((Right _):xs) = lefts' xs
lefts' ((Left x):xs) = (x:(lefts' xs))

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where 
    go (Right x) xs = (x:xs)
    go _ xs = xs

partition' :: [Either a b] -> ([a], [b])
partition' = foldr go ([], [])
  where 
    go (Left x) (lefts, rights) = ((x:lefts), rights)
    go (Right x) (lefts, rights) = (lefts, (x:rights))

eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' f _ (Left x) = f x
either' _ g (Right y) = g y

eitherMaybe'' = either' (\ _ -> Nothing) Just 

myIterate :: (a -> a) -> a -> [a]
myIterate produce initialElem = (initialElem:(myIterate produce initialElem))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case (f x) of
  Nothing -> []
  Just (y, z) -> (y : (myUnfoldr f z))


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = (x : (myUnfoldr (\y -> Just (f y, f y)) x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of 
  Nothing -> Leaf
  Just (l, n, r) -> Node (unfold f l) n (unfold f r)

treeBuildDown :: Integer -> BinaryTree Integer
treeBuildDown n = unfold iter n
  where
    iter 0 = Nothing
    iter i = Just ((i - 1), i, (i - 1))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\i -> if (i > n) then Nothing else Just ((i + 1), i, (i + 1))) 0

