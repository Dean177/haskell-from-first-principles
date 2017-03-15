import Data.Time
data DatabaseItem =
  DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbNumber 9001
  , DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbString "Hello, world!" 
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 9006
  , DbNumber 9024
  ]

isDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
isDbDate item times = case item of 
  DbDate time -> times ++ [time] 
  _ -> times

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr isDbDate []

isDbNum :: DatabaseItem -> [Integer] -> [Integer]
isDbNum (DbNumber x) xs = x:xs
isDbNum _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr isDbNum []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb databse = sum dbNumbers / fromIntegral length dbNumbers
  where dbNumbers = filterDbNumber databse 

  