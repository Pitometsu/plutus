{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Command (Command (..), ConfigCommand(..), NoConfigCommand(..)) where

import qualified Data.Aeson                              as JSON
import           GHC.Generics                            (Generic)
import           Plutus.PAB.Effects.Contract.ContractExe (ContractExe)
import           Wallet.Types                            (ContractInstanceId)

-- | A command for which a config.yaml file is required
data ConfigCommand =
    MockNode -- ^ Run the mock node service
    | MockWallet -- ^ Run the mock wallet service
    | ChainIndex -- ^ Run the chain index service
    | Metadata -- ^ Run the mock meta-data service
    | ForkCommands [ConfigCommand] -- ^ Fork  a list of commands
    | InstallContract ContractExe -- ^ Install a contract
    | ContractState ContractInstanceId -- ^ Display the contract identified by 'ContractInstanceId'
    | ReportContractHistory ContractInstanceId -- ^ Get the history of the contract identified by 'UUID'
    | ReportInstalledContracts -- ^ Get installed contracts
    | ReportActiveContracts -- ^ Get active contracts
    | PABWebserver -- ^ Run the PAB webserver
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON

data NoConfigCommand =
    Migrate { dbPath :: !FilePath } -- ^ Execute a database migration
    | PSGenerator -- ^ Generate purescript bridge code
          { outputDir :: !FilePath -- ^ Path to write generated code to
          }
    | WriteDefaultConfig -- ^ Write default logging configuration
          { outputFile :: !FilePath -- ^ Path to write configuration to
          }
    | StartSimulatorWebServer -- ^ Start a simulator with some predefined contracts, no interaction with external services
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON

-- | Commands that can be interpreted with 'runCliCommand'
data Command
    = WithoutConfig NoConfigCommand
    | WithConfig ConfigCommand
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON
