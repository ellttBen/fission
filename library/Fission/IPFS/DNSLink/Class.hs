module Fission.IPFS.DNSLink.Class (MonadDNSLink (..)) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.AWS.Route53.Class
import           Fission.URL.Types as URL
import           Fission.Prelude hiding (set)

class MonadRoute53 m => MonadDNSLink m where
  set    :: URL -> CID -> m (Either ServerError URL.DomainName)
  follow :: URL -> URL -> m (Either ServerError ())