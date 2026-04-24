-- Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}

module DA.Ledger.Services.UpdateService (
    getUpdateById,
    GetUpdateResponse,
    ) where

import DA.Ledger.GrpcWrapUtils
import DA.Ledger.LedgerService
import DA.Ledger.Types (Party, unParty)
import Data.Text.Lazy (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Network.GRPC.HighLevel.Generated
import qualified Proto3.Suite.Types as HsProtobuf

import qualified Com.Daml.Ledger.Api.V2.UpdateService as LL
import Com.Daml.Ledger.Api.V2.UpdateService (GetUpdateResponse)
import qualified Com.Daml.Ledger.Api.V2.TransactionFilter as TF

-- | Fetch a single update (transaction, reassignment, or topology transaction) by update-id.
-- The ledger projects the update to the supplied requesting parties and returns ledger effects
-- (create, consuming-exercise, non-consuming-exercise) when a party is a witness.
getUpdateById :: Text -> [Party] -> LedgerService GetUpdateResponse
getUpdateById updateId parties =
    makeLedgerService $ \timeout config mdm -> do
    withGRPCClient config $ \client -> do
        service <- LL.updateServiceClient client
        let LL.UpdateService{updateServiceGetUpdateById=rpc} = service
        let request = LL.GetUpdateByIdRequest
                { LL.getUpdateByIdRequestUpdateId = updateId
                , LL.getUpdateByIdRequestUpdateFormat = Just (ledgerEffectsFormat parties)
                }
        rpc (ClientNormalRequest request timeout mdm) >>= unwrap

-- | Build an UpdateFormat that requests transactions (ledger-effects shape), reassignments,
-- and topology events for the given set of parties, with a wildcard template filter.
ledgerEffectsFormat :: [Party] -> TF.UpdateFormat
ledgerEffectsFormat parties =
    TF.UpdateFormat
        { TF.updateFormatIncludeTransactions = Just TF.TransactionFormat
              { TF.transactionFormatEventFormat = Just (eventFormat parties)
              , TF.transactionFormatTransactionShape =
                    HsProtobuf.Enumerated (Right TF.TransactionShapeTRANSACTION_SHAPE_LEDGER_EFFECTS)
              }
        , TF.updateFormatIncludeReassignments = Just (eventFormat parties)
        , TF.updateFormatIncludeTopologyEvents = Just TF.TopologyFormat
              { TF.topologyFormatIncludeParticipantAuthorizationEvents = Nothing
              }
        }

eventFormat :: [Party] -> TF.EventFormat
eventFormat parties = TF.EventFormat
    { TF.eventFormatFiltersByParty = Map.fromList [ (unParty p, Just wildcardFilters) | p <- parties ]
    , TF.eventFormatFiltersForAnyParty = Nothing
    , TF.eventFormatVerbose = True
    }

wildcardFilters :: TF.Filters
wildcardFilters = TF.Filters
    { TF.filtersCumulative = Vector.singleton TF.CumulativeFilter
        { TF.cumulativeFilterIdentifierFilter = Just
            (TF.CumulativeFilterIdentifierFilterWildcardFilter
                TF.WildcardFilter { TF.wildcardFilterIncludeCreatedEventBlob = False })
        }
    }
