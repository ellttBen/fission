module Fission.App.Domain.Associator.Class
  ( Associator (..)
  , Errors
  ) where

import           Database.Esqueleto (insert_, insertUnique)

import           Fission.Prelude
import           Fission.Error
import           Fission.Models
import           Fission.Ownership
import           Fission.URL

import qualified Fission.App.Retriever    as App
import qualified Fission.App.Domain.Error as AppDomain
import qualified Fission.App.Domain.Types as AppDomain

import qualified Fission.Error as Error

type Errors = OpenUnion
  '[ AppDomain.AlreadyAssociated
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Associator m where
  associate ::
       UserId
    -> AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors ())

instance MonadIO m => Associator (Transaction m) where
  associate userId appId domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return <| relaxedLeft err

      Right (Entity _ app) ->
        case isOwnedBy userId app of
          False ->
            return . Error.openLeft <| ActionNotAuthorized @App userId

          True -> do
            insert_ AssociateAppDomainEvent
              { associateAppDomainEventAppId      = appId
              , associateAppDomainEventDomainName = domainName
              , associateAppDomainEventSubdomain  = maySubdomain
              , associateAppDomainEventInsertedAt = now
              }

            AppDomain
              { appDomainAppId        = appId
              , appDomainDomainName   = domainName
              , appDomainSubdomain    = maySubdomain
              , appDomainIsBareDomain = maybe (Just AppDomain.IsBare) (\_ -> Nothing) maySubdomain
              , appDomainInsertedAt   = now
              }
              |> insertUnique
              |> fmap \case
                Nothing -> Error.openLeft <| AppDomain.AlreadyAssociated appId domainName maySubdomain
                Just _  -> ok
