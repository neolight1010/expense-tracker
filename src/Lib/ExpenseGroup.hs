{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib.ExpenseGroup (ExpenseGroupSummary (..), expenseGroupSummary, ExpenseGroup, ExpenseEntry (..), PricedLabel) where

import qualified Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import Data.Yaml.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Lib.Item (ItemDefinitions, ItemId)
import qualified Lib.Item as Item
import Lib.Price (Price)

type ExpenseGroup = [ExpenseEntry]

data ExpenseEntry = ExpenseEntry {item :: ItemId, logs :: [Price]}
  deriving (Generic, Eq, Show)

instance FromJSON ExpenseEntry

instance ToJSON ExpenseEntry

data ExpenseGroupSummary = ExpenseGroupSummary {itemTotals :: [PricedLabel], categoryTotals :: [PricedLabel], total :: Price}
  deriving (Generic, Show, Eq)

instance ToJSON ExpenseGroupSummary

-- | A label with a price. Useful, for example, for category or item totals.
type PricedLabel = (String, Price)

expenseGroupSummary :: ItemDefinitions -> ExpenseGroup -> ExpenseGroupSummary
expenseGroupSummary itemDefinitions group =
  let itemExpensesMap = Map.fromListWith (+) (expenseEntryToTuple <$> group)
      itemTotals' = sortPricedLabels $ Map.toList itemExpensesMap

      categoryTotals' = sortPricedLabels $ aggregateCategoryTotals itemDefinitions group

      total' :: Price
      total' = sum $ map snd categoryTotals'
   in ExpenseGroupSummary itemTotals' categoryTotals' total'

expenseEntryToTuple :: ExpenseEntry -> (ItemId, Price)
expenseEntryToTuple (ExpenseEntry i p) = (i, sum p)

aggregateCategoryTotals :: ItemDefinitions -> ExpenseGroup -> [PricedLabel]
aggregateCategoryTotals itemDefinitions group = Map.toList $ Map.fromListWith (+) categoryExpensesList
  where
    categoryExpensesList = Data.Bifunctor.first findCategory . expenseEntryToTuple <$> group

    findCategory :: ItemId -> String
    findCategory itemId = maybe noCategory Item.category (Map.lookup itemId itemDefinitions)

sortPricedLabels :: [PricedLabel] -> [PricedLabel]
sortPricedLabels = List.sortOn (Ord.Down . snd)

noCategory :: String
noCategory = "[no-category]"
