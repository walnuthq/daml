-- Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}

module DA.Ledger.Services.CommandService (
    submitAndWaitForTransaction,
    SubmitAndWaitForTransactionResponse,
    -- Re-export proto command constructors so callers can build the `Commands`
    -- payload against the schema-aware encoder without importing proto-qualified
    -- modules everywhere.
    Commands(..),
    Command(..),
    CommandCommand(..),
    CreateCommand(..),
    ExerciseCommand(..),
    ExerciseByKeyCommand(..),
    CreateAndExerciseCommand(..),
    ) where

import DA.Ledger.GrpcWrapUtils
import DA.Ledger.LedgerService
import Network.GRPC.HighLevel.Generated
import qualified Proto3.Suite.Types as HsProtobuf

import qualified Com.Daml.Ledger.Api.V2.CommandService as LL
import Com.Daml.Ledger.Api.V2.CommandService (SubmitAndWaitForTransactionResponse)
import Com.Daml.Ledger.Api.V2.Commands
    ( Commands(..), Command(..), CommandCommand(..)
    , CreateCommand(..), ExerciseCommand(..)
    , ExerciseByKeyCommand(..), CreateAndExerciseCommand(..)
    )
import qualified Com.Daml.Ledger.Api.V2.TransactionFilter as TF
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Data.Text.Lazy (Text)

-- | Submit a pre-built `Commands` payload and block until the transaction is
-- sequenced. The server returns the resulting transaction (ledger-effects
-- shape, filtered to the submitting parties in `commands.actAs`).
submitAndWaitForTransaction :: Commands -> LedgerService SubmitAndWaitForTransactionResponse
submitAndWaitForTransaction commands =
    makeLedgerService $ \timeout config mdm -> do
    withGRPCClient config $ \client -> do
        service <- LL.commandServiceClient client
        let LL.CommandService{commandServiceSubmitAndWaitForTransaction=rpc} = service
        let txFmt = ledgerEffectsFormatFor (Vector.toList (commandsActAs commands))
        let request = LL.SubmitAndWaitForTransactionRequest
                { LL.submitAndWaitForTransactionRequestCommands = Just commands
                , LL.submitAndWaitForTransactionRequestTransactionFormat = Just txFmt
                }
        rpc (ClientNormalRequest request timeout mdm) >>= unwrap

-- | A TransactionFormat requesting LEDGER_EFFECTS events for the given
-- submitting parties. Identical in spirit to the format built by
-- `DA.Ledger.Services.UpdateService`, but flattened one layer for command
-- submission (which doesn't need the full UpdateFormat wrapper).
ledgerEffectsFormatFor :: [Text] -> TF.TransactionFormat
ledgerEffectsFormatFor parties = TF.TransactionFormat
    { TF.transactionFormatEventFormat = Just TF.EventFormat
          { TF.eventFormatFiltersByParty = Map.fromList
              [ (p, Just wildcardFilters) | p <- parties ]
          , TF.eventFormatFiltersForAnyParty = Nothing
          , TF.eventFormatVerbose = True
          }
    , TF.transactionFormatTransactionShape =
          HsProtobuf.Enumerated (Right TF.TransactionShapeTRANSACTION_SHAPE_LEDGER_EFFECTS)
    }

wildcardFilters :: TF.Filters
wildcardFilters = TF.Filters
    { TF.filtersCumulative = Vector.singleton TF.CumulativeFilter
        { TF.cumulativeFilterIdentifierFilter = Just
            (TF.CumulativeFilterIdentifierFilterWildcardFilter
                TF.WildcardFilter { TF.wildcardFilterIncludeCreatedEventBlob = False })
        }
    }
