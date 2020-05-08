module Fission.Web.Client.App (Update) where

import           Servant
import qualified Fission.Web.App.Update as App.Update
import           Fission.Web.Routes (AppPrefix)

import qualified Fission.Web.Auth.Types as Auth

type Update
  = AppPrefix
  :> Auth.HigherOrder
  :> App.Update.API
