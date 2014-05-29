module Linux.Arch.Aur where

data PkgInfo = PkgInfo { pkgIdOf          :: Int
                       , pkgNameOf        :: String
                         -- This should be `Version` and parsed nicely.
                       , pkgVersionOf     :: String
                       , pkgCategoryOf    :: Int
                       , pkgDescriptionOf :: String
                       , pkgUrlOf         :: String
                       , pkgVotesOf       :: Int
                       , isOutOfDate      :: Bool
                       , pkgMaintainerOf  :: String
                       , submissionDatOf  :: String  -- String?
                       , modifiedDateOf   :: String
                       , pkgTarballUrlOf  :: String
                       , pkgDependsOf     :: [String]
                       , pkgMakeDepsOf    :: [String]
                       , pkgOptDepsOf     :: [String]
                       , pkgConflictsOf   :: [String]
                       , pkgProvidesOf    :: [String] } deriving (Eq,Show)
