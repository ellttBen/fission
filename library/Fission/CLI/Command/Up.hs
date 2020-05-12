-- | File sync, IPFS-style
module Fission.CLI.Command.Up (cmd, up) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Options.Applicative
import           RIO.Directory

import           Network.IPFS
import qualified Network.IPFS.Add as IPFS

import           Fission.Prelude
import           Fission.Authorization.ServerDID
 
import           Fission.Web.Auth.Token
import           Fission.Web.Client as Client

import           Fission.Web.Client.App as App

import           Fission.CLI.Environment

import           Fission.CLI.Command.Types
import           Fission.CLI.Command.Up.Types as Up

import           Fission.CLI.Display.Error
import qualified Fission.CLI.Prompt.BuildDir as Prompt
 
import qualified Fission.CLI.DNS      as CLI.DNS
import qualified Fission.CLI.IPFS.Pin as CLI.Pin
import qualified Fission.CLI.Display.Error   as CLI.Error




import Database.Persist.Sql
import Fission.URL.Types
import Fission.App.URL.Class

-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , HasAppURL        m
  , ServerDID        m
  )
  => Command m Up.Options ()
cmd = Command
  { command     = "up"
  , description = "Keep your current working directory up"
  , argParser   = parseOptions
  , handler     = up -- FIXME needs to know the app's URL
  }

-- | Sync the current working directory to the server over IPFS
up ::
  ( MonadUnliftIO    m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , HasAppURL        m
  , ServerDID        m
  )
  -- => URL
  => Up.Options
  -> m ()
up Up.Options {..} = do
  ignoredFiles <- getIgnoredFiles
  toAdd        <- Prompt.checkBuildDir path
  absPath      <- liftIO $ makeAbsolute toAdd
  appURL       <- getAppURL

  logDebug $ "Starting single IPFS add locally of " <> displayShow absPath
  IPFS.addDir ignoredFiles absPath >>= putErrOr \cid -> do
    unless dnsOnly $
      sendRequestM (authClient (Proxy @App.Update) `withPayload` appURL `withPayload` cid) >>= \case
        Left err -> CLI.Error.put err "Server unable to sync data"
        Right _  -> return ()

    CLI.DNS.update cid >>= \case
      Left err -> CLI.Error.put err "DNS update unsuccessful"
      Right _  -> return ()

parseOptions :: Parser Up.Options
parseOptions = do
  dnsOnly <- switch $ mconcat
    [ long "dns-only"
    , help "Only update DNS (skip file sync)"
    ]

  path <- strArgument $ mconcat
    [ metavar "PATH"
    , help    "The file path of the assets or directory to sync"
    , value   "./"
    ]

  return Up.Options {..}

-- FIXME when updating, most should app's DNSLinks should follow, not set
