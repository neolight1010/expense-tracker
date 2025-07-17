{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Lib.Ledger
  ( Ledger (..),
    ExpenseGroups,
    decodeLedger,
    expenseGroupNames
  )
where

import qualified Data.Map as Map
import Data.Yaml (decodeEither')
import Data.Yaml.Aeson (FromJSON)
import GHC.Generics (Generic)

import qualified Lib.ExpenseGroup as ExpenseGroup

newtype Ledger = Ledger
  { expenses :: ExpenseGroups
  }
  deriving (Generic, Eq, Show)

instance FromJSON Ledger

type ExpenseGroups = Map.Map String ExpenseGroup.ExpenseGroup

expenseGroupNames :: Ledger -> [String]
expenseGroupNames = Map.keys . expenses

decodeLedger :: _ -> Either _ Ledger
decodeLedger = decodeEither'

