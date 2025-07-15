{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as ByteString
import qualified Iris
import Lib (decodeLedger)
import ListCommand (handleListCommand)
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
  let listCommand = Opt.command "list" (Opt.info (pure List) (Opt.progDesc "List all the expense groups"))
      summaryCommand = Opt.command "summary" (Opt.info (Summary <$> Opt.strArgument (Opt.metavar "GROUP" <> Opt.help "Group name to summarize")) (Opt.progDesc "Summarize an expense group"))
   in Options
        <$> Opt.hsubparser
          ( listCommand <> summaryCommand
          )

newtype Options = Options
  { optCommand :: Command
  }

data Command = List | Summary String
  deriving (Show)

handleCommand :: Command -> IO ()
handleCommand command = do
  ledgerFile <- ByteString.readFile defaultLedgerPath
  let ledger = decodeLedger ledgerFile

  case ledger of
    Right l ->
      case command of
        List -> handleListCommand l
        Summary groupName -> putStrLn ("TODO Implement summary command: " ++ groupName)
    Left _ -> putStrLn "Could not parse ledger file!"

app :: App ()
app = do
  Options {optCommand} <- Iris.asksCliEnv Iris.cliEnvCmd

  liftIO $ handleCommand optCommand

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
