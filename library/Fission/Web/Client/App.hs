-- | 

module Fission.Web.Client.App
  ( Register
  , Verify
  , WhoAmI
  , UpdatePK
  ) where

import           Servant
import qualified Fission.Web.User   as User
import           Fission.Web.Routes (UserPrefix)

type Update = AppPrefix :> App.RegisterRoute
