{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as ByteString
import qualified Iris
import Lib (Ledger, decodeLedger, expenseGroupNames)
import qualified Options.Applicative as Opt

defaultLedgerPath :: String
defaultLedgerPath = "ledger.yaml"

newtype App a = App
  { unApp :: Iris.CliApp Options () a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Iris.CliEnv Options ())
    )

appSettings :: Iris.CliEnvSettings Options ()
appSettings =
  Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Expense Tracker",
      Iris.cliEnvSettingsProgDesc = "A minimal, file-based expense tracker app.",
      Iris.cliEnvSettingsCmdParser = cmdParser
    }

cmdParser :: Opt.Parser Options
cmdParser =
  Options
    <$> Opt.hsubparser
      ( Opt.command "list" (Opt.info (pure List) (Opt.progDesc "List all the expense groups"))
      )

newtype Options = Options
  { optCommand :: Command
  }

data Command = List
  deriving (Show)

handleCommand :: Command -> IO ()
handleCommand command = do
  ledgerFile <- ByteString.readFile defaultLedgerPath
  let ledger = decodeLedger ledgerFile

  case ledger of
    Right l ->
      case command of
        List -> handleListCommand l
    Left _ -> putStrLn "Could not parse ledger file!"

handleListCommand :: Ledger -> IO ()
handleListCommand ledger =
  let groupLines = (unlines . expenseGroupNames) ledger
   in putStrLn groupLines

app :: App ()
app = do
  Options {optCommand} <- Iris.asksCliEnv Iris.cliEnvCmd

  liftIO $ handleCommand optCommand

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
