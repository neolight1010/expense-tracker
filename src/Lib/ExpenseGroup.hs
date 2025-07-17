{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib.ExpenseGroup (ExpenseGroupSummary (..), expenseGroupSummary, ExpenseGroup, ExpenseEntry (..)) where

import qualified Data.Map as Map
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)
import Lib.Item (ItemId)
import Lib.Price (Price)

type ExpenseGroup = [ExpenseEntry]

data ExpenseEntry = ExpenseEntry {item :: ItemId, price :: Price}
  deriving (Generic, Eq, Show)

instance FromJSON ExpenseEntry

newtype ExpenseGroupSummary = ExpenseGroupSummary [ExpenseEntry]
  deriving (Show, Eq)

expenseGroupSummary :: ExpenseGroup -> ExpenseGroupSummary
expenseGroupSummary group =
  let groupMap = Map.fromListWith (+) (expenseEntryToTuple <$> group)
   in ExpenseGroupSummary (expenseEntryFromTuple <$> Map.toList groupMap)

expenseEntryToTuple :: ExpenseEntry -> (ItemId, Price)
expenseEntryToTuple (ExpenseEntry i p) = (i, p)

expenseEntryFromTuple :: (ItemId, Price) -> ExpenseEntry
expenseEntryFromTuple (i, p) = ExpenseEntry i p
