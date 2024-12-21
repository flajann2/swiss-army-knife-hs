{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_swiss_army_knife_hs (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "swiss_army_knife_hs"
version :: Version
version = Version [0,0,0,0] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = "2024 Fred Mitchell & Atomlogik"
homepage :: String
homepage = "https://github.com/flajann2/swiss-army-knife-hs#readme"
