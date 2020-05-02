module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist
import           Network.IPFS.CID.Types

import           Fission.Prelude
import           Fission.Models

import           Fission.Error as Error

type Errors = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App
   ]

class Monad m => Modifier m where
  updateCID :: UserId -> AppId -> CID -> UTCTime -> m (Either Errors ())

instance MonadIO m => Modifier (Transaction m) where
  updateCID _ appId newCID now = do
    update appId [AppCid =. newCID]
    insert $ SetAppCIDEvent appId newCID now
    return ok
