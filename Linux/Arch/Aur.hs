{-# LANGUAGE OverloadedStrings #-}

module Linux.Arch.Aur
       ( -- * QUERIES
         search
       , info
       , multiinfo
       , msearch ) where

import Linux.Arch.Aur.Types

import Control.Lens
import Data.Text
import Data.Aeson.Lens (_String, key)
import Network.Wreq

---

rpcUrl :: String
rpcUrl = "https://aur.archlinux.org/rpc.php?"
         ++ apiVersion
         ++ "&type=multiinfo&"

apiVersion = "v=2"

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

--foo :: IO ()
foo = do
  r <- getWith opts rpcUrl
  return $ r ^. responseBody -- . key "url" . _String
      where opts = defaults & param "arg[]" .~ ["aura","aura-bin"]
