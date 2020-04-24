module Fission.App.Creator.Class
  ( Creator (..)
  , Errors
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Domain   as App.Domain

type Errors = OpenUnion
  '[ ServerError
   , App.Domain.AlreadyAssociated
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Creator m where
  create :: UserId -> CID -> UTCTime -> m (Either Errors (AppId, Subdomain))
