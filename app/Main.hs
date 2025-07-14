{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Iris
import qualified Options.Applicative as Opt
import Control.Monad.Reader (MonadReader)

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
  Options <$> Opt.hsubparser
    ( Opt.command "list" (Opt.info (pure List) (Opt.progDesc "List all the expense groups"))
    )

newtype Options = Options {
  optCommand :: Command
}

data Command = List
  deriving (Show)

app :: App ()
app = do
  Options {optCommand} <- Iris.asksCliEnv Iris.cliEnvCmd

  liftIO $ print optCommand

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
