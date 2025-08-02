{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lib.Ledger
  ( Ledger (..),
    ExpenseGroups,
    decodeLedger,
    expenseGroupNames,
    expenseGroup,
    expenseGroupSummary,
  )
where

import qualified Data.Map as Map
import Data.Yaml (decodeEither')
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Lib.ExpenseGroup as ExpenseGroup
import Lib.Item (ItemDefinitions)

data Ledger = Ledger
  { expenses :: ExpenseGroups,
    items :: ItemDefinitions
  }
  deriving (Generic, Eq, Show)

instance FromJSON Ledger

type ExpenseGroups = Map.Map String ExpenseGroup.ExpenseGroup

expenseGroupNames :: Ledger -> [String]
expenseGroupNames = Map.keys . expenses

decodeLedger :: _ -> Either _ Ledger
decodeLedger = decodeEither'

expenseGroup :: Ledger -> String -> Maybe ExpenseGroup.ExpenseGroup
expenseGroup (Ledger {expenses}) = flip Map.lookup expenses

expenseGroupSummary :: Ledger -> String -> Maybe ExpenseGroup.ExpenseGroupSummary
expenseGroupSummary ledger = fmap ExpenseGroup.expenseGroupSummary . expenseGroup ledger
