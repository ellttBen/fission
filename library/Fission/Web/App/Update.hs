module Fission.Web.App.Update
  ( API
  , update
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import qualified Fission.App       as App
import           Fission.Web.Error as Web.Error

import           Fission.URL.Types

type API
  =  Summary     "Set app content"
  :> Description "Update the content (CID) for an app"
  :> Capture     "App URL" URL
  :> Capture     "New CID" CID
  :> PatchAccepted '[JSON] NoContent

update ::
  ( MonadLogger  m
  , MonadThrow   m
  , MonadTime    m
  , App.Modifier m
  )
  => Authorization
  -> ServerT API m
update Authorization {about = Entity userId _} url newCID = do
  now <- currentTime
  Web.Error.ensureM $ App.updateCID userId url newCID now
  return NoContent
