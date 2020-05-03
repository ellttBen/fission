-- | App configuration for Fission apps
module Fission.Environment.WebApp.Types (Environment (..)) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.AWS.Types as AWS
import           Fission.URL.Types as URL

data Environment = Environment
  { baseAppDomainName :: !URL.DomainName -- ^ Default domain name
  , appPlaceholder    :: !CID            -- ^ Initial CID
  , liveDriveURL      :: !URL
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "WebApp.Environment" \obj -> do
    baseAppDomainName <- obj .: "base_domain_name"
    appPlaceholder    <- obj .: "placeholder_cid"
    liveDriveURL      <- obj .: "live_drive_url"

    return Environment {..}
