-- | App configuration for Fission apps
module Fission.Environment.WebApp.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.AWS.Types as AWS
import qualified Fission.URL.Types as URL

data Environment = Environment
  { baseAppDomainName :: !URL.DomainName -- ^ Default domain name
  , appPlaceholder    :: !CID            -- ^ Initial CID
  , appZoneID         :: !AWS.ZoneID     -- ^ Hosted Zone for user apps
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "WebApp.Environment" \obj -> do
    baseAppDomainName <- obj .: "base_domain_name"
    appPlaceholder    <- obj .: "placeholder_cid"
    appZoneID         <- obj .: "aws_zone_id"

    return Environment {..}
