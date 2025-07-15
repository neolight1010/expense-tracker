{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lib
  ( Ledger (..),
    ExpenseGroup,
    ExpenseEntry (..),
    ExpenseGroups,
    ExpenseGroupSummary (..),
    decodeLedger,
    expenseGroupNames,
    expenseGroupSummary,
  )
where

import qualified Data.Map as Map
import Data.Yaml (decodeEither')
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype Ledger = Ledger
  { expenses :: ExpenseGroups
  }
  deriving (Generic, Eq, Show)

instance FromJSON Ledger

type ExpenseGroups = Map.Map String ExpenseGroup

type ExpenseGroup = [ExpenseEntry]

data ExpenseEntry = ExpenseEntry {item :: ItemId, price :: Price}
  deriving (Generic, Eq, Show)

instance FromJSON ExpenseEntry

expenseEntryToTuple :: ExpenseEntry -> (ItemId, Price)
expenseEntryToTuple (ExpenseEntry i p) = (i, p)

expenseEntryFromTuple :: (ItemId, Price) -> ExpenseEntry
expenseEntryFromTuple (i, p) = ExpenseEntry i p

type ItemId = String

type Price = Float

decodeLedger :: _ -> Either _ Ledger
decodeLedger = decodeEither'

expenseGroupNames :: Ledger -> [String]
expenseGroupNames = Map.keys . expenses

newtype ExpenseGroupSummary = ExpenseGroupSummary [ExpenseEntry]
  deriving (Show, Eq)

expenseGroupSummary :: ExpenseGroup -> ExpenseGroupSummary
expenseGroupSummary group =
  let groupMap = Map.fromListWith (+) (expenseEntryToTuple <$> group)
   in ExpenseGroupSummary (expenseEntryFromTuple <$> Map.toList groupMap)
