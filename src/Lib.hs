{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( Ledger (..),
    ExpenseList,
    ExpenseEntry (..),
    ExpenseGroups,
  )
where

import Data.Map (Map)
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype Ledger = Ledger
  { expenses :: ExpenseGroups
  }
  deriving (Generic, Eq, Show)

instance FromJSON Ledger

type ExpenseGroups = Map String ExpenseList

type ExpenseList = [ExpenseEntry]

data ExpenseEntry = ExpenseEntry
  { item :: ItemId,
    price :: Price
  }
  deriving (Generic, Eq, Show)

instance FromJSON ExpenseEntry

type ItemId = String

type Price = Float
