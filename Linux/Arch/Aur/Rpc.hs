{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Linux.Arch.Aur.Rpc
-- Copyright : (c) Colin Woodbury, 2014,2015
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Linux.Arch.Aur.Rpc
       ( -- * AUR4 Queries
         info
       , multiinfo
       , search
       , msearch
       -- * AUR4 Queries as JSON
       , infoJ
       , multiinfoJ
       , searchJ
       , msearchJ
       -- * Explicit API version/endpoint
       , info'
       , multiinfo'
       , search'
       , msearch'
       -- * Explicit API version/endpoint as JSON
       , infoJ'
       , multiinfoJ'
       , searchJ'
       , msearchJ'
       -- * Pretty JSON
       , pretty) where

import           Linux.Arch.Aur.Types

import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Aeson (Value(..), FromJSON, Result(..), fromJSON)
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens (AsValue, key, nth)
import           Data.Maybe (catMaybes)
import           Data.Text
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding
import qualified Data.Vector as V
import           Network.Wreq

---

aurUrl :: String
aurUrl = "https://aur.archlinux.org/rpc.php?"

aur4Url :: String
aur4Url = "https://aur4.archlinux.org/rpc.php?"

-- | Returns all information about one package.
info :: MonadIO m => Text -> m (Maybe AurInfo)
info p = (>>= extract) <$> infoJ p

-- | Like `info`, but can handle requests for multiple packages at once.
-- More efficient than using `info` multiple times.
multiinfo :: MonadIO m => [Text] -> m [AurInfo]
multiinfo ps = mapArray <$> multiinfoJ ps

-- | Yields any matches to the input as `AurInfo`, but
-- doesn't include dependency information.
search :: MonadIO m => Text -> m [AurInfo]
search s = mapArray <$> searchJ s

-- | Search the AUR by Maintainer name.
msearch :: MonadIO m => Text -> m [AurInfo]
msearch m = mapArray <$> msearchJ m

-- | `info` call as Haskellised JSON.
infoJ :: MonadIO m => Text -> m (Maybe Value)
infoJ p = (>>= (^? nth 0)) <$> multiinfoJ [p]

-- | `multiinfo` call as Haskellised JSON.
multiinfoJ :: MonadIO m => [Text] -> m (Maybe Value)
multiinfoJ ps = multiinfoJ' ps aur4Url "4"

-- | `search` call as Haskellised JSON.
searchJ :: MonadIO m => Text -> m (Maybe Value)
searchJ s = searchJ' s aur4Url "4"

-- | `msearch` call as Haskellised JSON.
msearchJ :: MonadIO m => Text -> m (Maybe Value)
msearchJ m = msearchJ' m aur4Url "4"

-- | `info` with explicit RPC endpoint and API version.
info' :: MonadIO m => Text -> String -> Text -> m (Maybe AurInfo)
info' p url ver = (>>= extract) <$> infoJ' p url ver

-- | `multiinfo` with explicit RPC endpoint and API version.
multiinfo' :: MonadIO m => [Text] -> String -> Text -> m [AurInfo]
multiinfo' ps url ver = mapArray <$> multiinfoJ' ps url ver

search' :: MonadIO m => Text -> String -> Text -> m [AurInfo]
search' s url ver = mapArray <$> searchJ' s url ver

msearch' :: MonadIO m => Text -> String -> Text -> m [AurInfo]
msearch' m url ver = mapArray <$> msearchJ' m url ver

infoJ' :: MonadIO m => Text -> String -> Text -> m (Maybe Value)
infoJ' p url ver = (>>= (^? nth 0)) <$> multiinfoJ' [p] url ver

multiinfoJ' :: MonadIO m => [Text] -> String -> Text -> m (Maybe Value)
multiinfoJ' ps url ver = rpc "multiinfo" ps "arg[]" url ver

searchJ' :: MonadIO m => Text -> String -> Text -> m (Maybe Value)
searchJ' s url ver = rpc "search" [s] "arg" url ver

msearchJ' :: MonadIO m => Text -> String -> Text -> m (Maybe Value)
msearchJ' m url ver = rpc "msearch" [m] "arg" url ver

-- | Call the RPC.
-- Doesn't fail elegantly when there is a connection failure.
rpc :: MonadIO m => Text -> [Text] -> Text -> String -> Text -> m (Maybe Value)
rpc method args argLabel url ver = liftIO (rpcResults <$> getWith opts url)
    where opts = defaults & param "type"   .~ [method]
                          & param argLabel .~ args
                          & param "v"      .~ [ver]

rpcResults :: AsValue r => Response r -> Maybe Value
rpcResults r = r ^? responseBody . key "results"

-- | Conversion of JSON to nicely formatted Text.
pretty :: Value -> Text
pretty = TL.toStrict . decodeUtf8 . encodePretty

--------------------
-- UTILITY FUNCTIONS
--------------------
extract :: FromJSON a => Value -> Maybe a
extract = f . fromJSON
    where f (Success x) = Just x
          f _           = Nothing

-- There must be a better way to do this.
-- Use lenses, child.
mapArray :: FromJSON a => Maybe Value -> [a]
mapArray (Just (Array v)) = catMaybes . V.toList . V.map extract $ v
mapArray _                = []
