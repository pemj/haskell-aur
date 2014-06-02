{-# LANGUAGE OverloadedStrings #-}

module Linux.Arch.Aur where

import Linux.Arch.Aur.Types

import Data.Text

---

rpcUrl :: Text
rpcUrl = "https://aur.archlinux.org/rpc.php"

-- QUERIES
-- | Yields any matches to the input as `AurInfo`, but
-- doesn't include dependency information.
search :: Text -> IO [AurInfo]
search = undefined

-- | Returns all information about one package.
info :: Text -> IO (Maybe AurInfo)
info = undefined

-- | Like `info`, but can handle requests for multiple packages at once.
-- More efficient than using `info` multiple times.
multiinfo :: [Text] -> IO [AurInfo]
multiinfo = undefined

-- | Search the AUR by Maintainer name.
msearch :: Text -> IO [AurInfo]
msearch = undefined
