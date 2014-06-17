{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Linux.Arch.Aur.Rpc
-- Copyright : (c) Colin Woodbury, 2014
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Linux.Arch.Aur.Pkgbuild
    ( pkgbuild
    , pkgbuildUrl ) where

import Control.Applicative ((<$>))
import Control.Lens ((^?))
import Control.Monad ((>=>))
import Data.Monoid ((<>))
import Data.Text hiding (take)
import Data.Text.Lazy.Encoding
import Network.Wreq
import System.FilePath ((</>))

import qualified Data.Text.Lazy as TL

---

baseUrl :: String
baseUrl = "https://aur.archlinux.org/packages/"

-- | The location of a given package's PKGBUILD on the AUR servers.
pkgbuildUrl :: String -> String
pkgbuildUrl p = baseUrl </> take 2 p </> p </> "PKGBUILD"

-- | The PKGBUILD of a given package, retrieved from the AUR servers.
pkgbuild :: String -> IO (Maybe Text)
pkgbuild p = (rb >=> txt) <$> get (pkgbuildUrl p)
    where rb  = (^? responseBody)
          txt = Just . TL.toStrict . decodeUtf8
