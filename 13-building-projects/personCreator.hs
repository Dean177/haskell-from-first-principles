type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ "Agewas:"++ show age

gimmiePerson :: IO ()
gimmiePerson = do
  putStrLn "Enter name"
  enteredName <- getLine
  putStrLn "Enter age"
  age <- getLine
  let parsedAge = read age
  case (mkPerson enteredName parsedAge) of
    Left err -> print err
    Right person -> do
      putStrLn "It flippin worked"
      print person
