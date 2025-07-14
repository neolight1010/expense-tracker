{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lib
  ( Ledger (..),
    ExpenseList,
    ExpenseEntry (..),
    ExpenseGroups,
    decodeLedger
  )
where

import Data.Map (Map)
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)
import Data.Yaml (decodeEither')

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

decodeLedger :: _ -> Either _ Ledger
decodeLedger = decodeEither'
