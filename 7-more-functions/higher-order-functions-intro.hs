data Employee = 
  Intern |
  Developer |
  Director |
  ProjectManager 
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss boss employee  = 
  putStrLn $ show boss ++ " is the bosser of " ++ show employee 

employeeRank :: (Employee -> Employee -> Ordering) ->
  Employee -> Employee -> IO ()

devTopOrd :: Employee -> Employee -> Ordering
devTopOrd Developer Developer = EQ
devTopOrd Developer _ = GT
devTopOrd _ Developer = LT
devTopOrd e e' = compare e e'

employeeRank orderer some other = 
  case orderer some other of
    GT -> reportBoss some other
    EQ -> putStrLn "They are peers"
    LT -> (flip reportBoss) some other
