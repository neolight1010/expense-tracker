{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib.ExpenseGroup (ExpenseGroupSummary (..), expenseGroupSummary, ExpenseGroup, ExpenseEntry (..)) where

import qualified Data.Map as Map
import Data.Yaml.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Lib.Item (ItemId, ItemDefinitions)
import qualified Data.Bifunctor
import qualified Lib.Item as Item
import Lib.Price (Price)

type ExpenseGroup = [ExpenseEntry]

data ExpenseEntry = ExpenseEntry {item :: ItemId, logs :: [Price]}
  deriving (Generic, Eq, Show)

instance FromJSON ExpenseEntry
instance ToJSON ExpenseEntry

data ExpenseGroupSummary = ExpenseGroupSummary { itemTotals :: [(String, Price)], categoryTotals :: [(String, Price)] }
  deriving (Generic, Show, Eq)

instance ToJSON ExpenseGroupSummary

expenseGroupSummary :: ItemDefinitions -> ExpenseGroup -> ExpenseGroupSummary
expenseGroupSummary itemDefinitions group =
  let itemExpensesMap = Map.fromListWith (+) (expenseEntryToTuple <$> group)
      itemTotals' = Map.toList itemExpensesMap

      findCategory :: ItemId -> String
      findCategory itemId = maybe noCategory Item.category (Map.lookup itemId itemDefinitions)

      categoryExpensesList = Data.Bifunctor.first findCategory . expenseEntryToTuple <$> group
      categoryTotals' = Map.toList $ Map.fromListWith (+) categoryExpensesList

   in ExpenseGroupSummary itemTotals' categoryTotals'

expenseEntryToTuple :: ExpenseEntry -> (ItemId, Price)
expenseEntryToTuple (ExpenseEntry i p) = (i, sum p)

noCategory :: String
noCategory = "[no-category]"
