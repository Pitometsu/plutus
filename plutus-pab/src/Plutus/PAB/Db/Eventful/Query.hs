{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Queries are views of the database. Or if you prefer, folds over the event store.
--
-- In 'eventful' they are implemented as 'Projection' types which retain
-- a memory of the last event they saw, such that if you rerun a
-- projection, it will only process new events, rather than
-- recalculating the fold from scratch.
module Plutus.PAB.Db.Eventful.Query
    ( nullProjection
    -- * Queries related to the installed and active contracts
    , installedContractsProjection
    -- * Queries related to contract instances
    , contractState
    , contractDefinition
    ) where

import           Control.Lens
import           Data.Aeson                 (Value)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text.Prettyprint.Doc  (Pretty, pretty)
import           Eventful                   (Projection (Projection), StreamEvent (StreamEvent), StreamProjection,
                                             projectionEventHandler, projectionMapMaybe, projectionSeed,
                                             streamProjectionState)
import           Plutus.Contract.State      (ContractResponse)
import           Plutus.PAB.Events          (PABEvent (InstallContract, UpdateContractInstanceState))
import           Plutus.PAB.Events.Contract (ContractInstanceId, ContractPABRequest)
import           Plutus.PAB.Webserver.Types (ContractActivationArgs (..))

-- | The empty projection. Particularly useful for commands that have no 'state'.
nullProjection :: Projection () event
nullProjection = contramap (const ()) monoidProjection

-- | A projection that just accumulates any monoid you supply.
-- This is particulatly useful when combined with function that filters down interesting events using 'projectionMapMaybe':
--
-- @
-- allNames :: Projection [Text] Event
-- allNames = projectionMapMaybe extractName monoidProjection
--   where
--     extractName (CreateUser name dateOfBirth) = Just [name]
--     extractName (ChristenShip name tonnage)   = Just [name]
--     extractName _                             = Nothing
-- @
monoidProjection :: Monoid m => Projection m m
monoidProjection =
    Projection {projectionSeed = mempty, projectionEventHandler = mappend}

-- | Similar to 'monoidProjection', but for accumulating sets instead of monoids.
setProjection :: Ord a => Projection (Set a) a
setProjection = contramap Set.singleton monoidProjection

------------------------------------------------------------
-- | The Pretty instance for 'StreamProjection' just pretty prints its resulting 'state'.
instance Pretty state =>
         Pretty (StreamProjection key position state event) where
    pretty = pretty . streamProjectionState

-- | The last known state of the contract.
contractState :: forall t key position. Projection (Map ContractInstanceId (ContractResponse Value Value Value ContractPABRequest)) (StreamEvent key position (PABEvent t))
contractState =
    let projectionEventHandler :: Map ContractInstanceId (ContractResponse Value Value Value ContractPABRequest) -> StreamEvent key position (PABEvent t) -> Map ContractInstanceId (ContractResponse Value Value Value ContractPABRequest)
        projectionEventHandler oldMap = \case
            (StreamEvent _ _ (UpdateContractInstanceState _ i s)) ->
                Map.union (Map.singleton i s) oldMap
            _ -> oldMap

    in Projection
        { projectionSeed = Map.empty
        , projectionEventHandler
        }

-- | The definition of the contract.
contractDefinition :: forall t key position. Projection (Map ContractInstanceId (ContractActivationArgs t)) (StreamEvent key position (PABEvent t))
contractDefinition =
    let projectionEventHandler :: Map ContractInstanceId (ContractActivationArgs t) -> StreamEvent key position (PABEvent t) -> Map ContractInstanceId (ContractActivationArgs t)
        projectionEventHandler oldMap = \case
            (StreamEvent _ _ (UpdateContractInstanceState d i _)) ->
                Map.union (Map.singleton i d) oldMap
            _ -> oldMap

    in Projection
        { projectionSeed = Map.empty
        , projectionEventHandler
        }

-- | Set of all contracts that have been installed.
installedContractsProjection ::
    forall t key position.
    Ord t => Projection (Set t) (StreamEvent key position (PABEvent t))
installedContractsProjection = projectionMapMaybe contractPaths setProjection
  where
    contractPaths (StreamEvent _ _ (InstallContract contract)) = Just contract
    contractPaths _                                            = Nothing
