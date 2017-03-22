data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String
data Garden =
  Garden Gardener FlowerType
  deriving Show


data GardenN = 
  GardeniaN Gardener 
  | DaisyN Gardener
  | RoseN Gardener
  | LilacN Gardener
  deriving Show

data OperatingSystem =
  GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)

data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer { os = operatingSystem, lang = language }
  | operatingSystem <- allOperatingSystems
  , language <- allLanguages
  ]


  