module Lib
  ( Item (..),
    Expenses (..),
    expensesTotal,
  )
where

newtype Item = Item String
  deriving (Show)

newtype Expenses = Expenses [(Item, Float)]
  deriving (Show)

expensesTotal :: Expenses -> Float
expensesTotal (Expenses expenses) = sum $ map snd expenses
