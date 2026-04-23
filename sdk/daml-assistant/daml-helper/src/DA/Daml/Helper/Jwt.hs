-- Copyright (c) 2025 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.Helper.Jwt
    ( JwtCreateOptions(..)
    , JwtMode(..)
    , runJwtCreate
    , encodeJwt
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Web.JWT as JWT

data JwtMode
    = UserMode { jwtUser :: T.Text }
    | LedgerApiMode
        { jwtActAs :: [T.Text]
        , jwtLedgerId :: T.Text
        , jwtApplicationId :: T.Text
        }

data JwtCreateOptions = JwtCreateOptions
    { jwtMode :: JwtMode
    , jwtScope :: T.Text
    , jwtSecret :: T.Text
    }

runJwtCreate :: JwtCreateOptions -> IO ()
runJwtCreate JwtCreateOptions{..} =
    TIO.putStrLn (encodeJwt jwtMode jwtScope jwtSecret)

encodeJwt :: JwtMode -> T.Text -> T.Text -> T.Text
encodeJwt mode scope secret =
    JWT.encodeSigned
        (JWT.EncodeHMACSecret (TE.encodeUtf8 secret))
        mempty
        (claimsFor mode scope)

claimsFor :: JwtMode -> T.Text -> JWT.JWTClaimsSet
claimsFor (UserMode user) scope =
    mempty
        { JWT.sub = JWT.stringOrURI user
        , JWT.unregisteredClaims = JWT.ClaimsMap $
            Map.fromList [("scope", Aeson.String scope)]
        }
claimsFor (LedgerApiMode actAs ledgerId appId) _ =
    mempty
        { JWT.unregisteredClaims = JWT.ClaimsMap $
            Map.fromList
                [ ( "https://daml.com/ledger-api"
                  , Aeson.Object $ KM.fromList
                      [ (Aeson.Key.fromText "ledgerId", Aeson.String ledgerId)
                      , (Aeson.Key.fromText "applicationId", Aeson.String appId)
                      , (Aeson.Key.fromText "actAs", Aeson.Array $ V.fromList (map Aeson.String actAs))
                      ]
                  )
                ]
        }
