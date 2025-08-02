{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib.Item (ItemId, ItemDefinitions, Item (..)) where

import qualified Data.Map as Map
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

type ItemId = String

type ItemDefinitions = Map.Map String Item

data Item = Item { category :: String }
  deriving (Generic, Eq, Show)

instance FromJSON Item
