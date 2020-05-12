-- | FIXME description

module Fission.App.URL.Class (HasAppURL (..)) where

import Fission.Prelude
import Fission.URL.Types

-- FIXME description
class Monad m => HasAppURL m where
  getAppURL :: m URL
