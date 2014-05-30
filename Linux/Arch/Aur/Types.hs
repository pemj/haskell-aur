module Linux.Arch.Aur where

data AurInfo = AurInfo { aurIdOf          :: Int
                       , aurNameOf        :: String
                         -- This should be `Version` and parsed nicely.
                       , aurVersionOf     :: String
                       , aurCategoryOf    :: Int
                       , aurDescriptionOf :: String
                       , urlOf            :: String
                       , aurVotesOf       :: Int
                       , isOutOfDate      :: Bool
                       , aurMaintainerOf  :: String
                       , submissionDatOf  :: String  -- String?
                       , modifiedDateOf   :: String
                       , aurTarballUrlOf  :: String
                       , dependsOf        :: [String]
                       , makeDepsOf       :: [String]
                       , optDepsOf        :: [String]
                       , conflictsOf      :: [String]
                       , providesOf       :: [String] } deriving (Eq,Show)
