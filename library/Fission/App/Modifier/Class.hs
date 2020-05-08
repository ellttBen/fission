module Fission.App.Modifier.Class
  ( Modifier (..)
  , Errors
  ) where

import           Database.Persist as Persist
import           Network.IPFS.CID.Types

import           Fission.Prelude hiding (on)
import           Fission.Models
import           Fission.Ownership
import           Fission.URL.Types

import           Fission.Error as Error

type Errors = OpenUnion
  '[ NotFound App
   , NotFound AppDomain

   , ActionNotAuthorized App
   ]

class Monad m => Modifier m where
  updateCID :: UserId -> URL -> CID -> UTCTime -> m (Either Errors AppId)

instance MonadIO m => Modifier (Transaction m) where
  updateCID userId URL {..} newCID now = do
    mayAppDomain <- Persist.selectFirst
      [ AppDomainDomainName ==. domainName
      , AppDomainSubdomain  ==. subdomain
      ] []

    case mayAppDomain of
      Nothing ->
        return . Error.openLeft $ NotFound @AppDomain

      Just (Entity _ AppDomain {appDomainAppId = appId}) ->
        Persist.get appId >>= \case
          Nothing ->
            return . Error.openLeft $ NotFound @App

          Just app ->
            if isOwnedBy userId app
              then do
                update appId [AppCid =. newCID]
                insert $ SetAppCIDEvent appId newCID now
                return $ Right appId

              else
                return . Error.openLeft $ ActionNotAuthorized @App userId
