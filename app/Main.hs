{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Iris

newtype App a = App
  { unApp :: Iris.CliApp () () a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

appSettings :: Iris.CliEnvSettings () ()
appSettings =
  Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Expense Tracker",
      Iris.cliEnvSettingsProgDesc = "A minimal, file-based expense tracker app."
    }

app :: App ()
app = liftIO $ putStrLn "Hello, World!"

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
