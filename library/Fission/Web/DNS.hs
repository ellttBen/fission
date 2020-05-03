module Fission.Web.DNS
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import           Fission.URL                as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type API
  =  Summary "Set account's DNSLink"
  :> Description "DEPRECATED ⛔ Set account's DNSLink to a CID"
  :> Capture "cid" CID
  :> PutAccepted '[PlainText, OctetStream] DomainName

-- Deprecated
server :: (MonadLogger m, MonadDNSLink m) => Authorization -> ServerT API m
server Authorization {about = Entity _ User {userUsername = Username rawUN}} cid = do
  void . Web.Err.ensureM $ DNSLink.set url cid
  return . DomainName $ rawUN <> "fission.name"
  where
    url = URL
      { domainName = DomainName "fission.name"
      , subdomain  = Just $ Subdomain rawUN
      }
