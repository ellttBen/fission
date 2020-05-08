module Fission.IPFS.DNSLink.Class (MonadDNSLink (..)) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude hiding (set)

import           Fission.URL.Types
import           Fission.AWS.Route53.Class

class MonadRoute53 m => MonadDNSLink m where
  set ::
       URL
    -> CID
    -> m (Either ServerError URL)

  follow ::
       URL -- ^ Follower
    -> URL -- ^ Followee
    -> m (Either ServerError ())
