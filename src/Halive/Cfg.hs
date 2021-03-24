{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Halive.Cfg (
      Cfg(..)
    , decodeCfg
    ) where

import Halive.SubHalive (Extension)
import Halive.Args (FileType)
import Data.Aeson (eitherDecode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy.Char8 as BL (pack)

data Cfg = Cfg {
      cfgFileTypes :: [FileType]
    , cfgMainFilePath :: FilePath
    , cfgIncludeDirs :: [FilePath]
    , cfgExtensions :: [Extension]
    , cfgDisableExtensions :: [Extension]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''Extension)
$(deriveJSON defaultOptions ''Cfg)

decodeCfg :: String -> Either String Cfg
decodeCfg str = eitherDecode $ BL.pack str
