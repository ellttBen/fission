module Fission.Types
  ( Fission (..)
  , module Fission.Config.Types
  ) where

import qualified Data.Aeson as JSON
import           Fission.URL
import           Fission.IPFS.DNSLink as DNSLink
import           Fission.User.Username.Types
 
import           Database.Persist as Persist

import           Control.Monad.Catch
import qualified Database.Persist.Sql as SQL
import qualified Fission.App as App

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Fission.Platform.Heroku.AddOn.Creator as Heroku.AddOn

import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Network.AWS as AWS hiding (Request)
import           Network.AWS.Route53
 
import qualified Network.IPFS               as IPFS
import qualified Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as IPFS.Process
import qualified Network.IPFS.Process       as IPFS
import qualified Network.IPFS.Peer          as Peer

import           Fission.Prelude
import           Fission.Config.Types
import           Fission.Models
import           Fission.Error as Error

import           Fission.Ownership

import           Fission.Web.Types
import qualified Fission.Web.Error as Web.Error
import qualified Fission.App.Creator as App

import           Fission.IPFS.Linked

import           Fission.Authorization.Types
import           Fission.URL as URL
import           Fission.AWS as AWS

import           Fission.Platform.Heroku.Types as Heroku

import           Fission.Web.Auth       as Auth
import qualified Fission.Web.Auth.DID   as Auth.DID
import qualified Fission.Web.Auth.Token as Auth.Token

import           Fission.Web.Server.Reflective as Reflective
import           Fission.Web.Handler

import           Fission.User.DID.Types
import qualified Fission.User          as User
import           Fission.User.Creator.Class

import           Fission.Web.Auth.Token.Basic.Class
import           Fission.Web.Auth.Token.JWT.Resolver as JWT
 
import           Fission.Authorization.ServerDID.Class

import           Fission.App.Content as App.Content
import           Fission.App.Domain  as App.Domain

-- | The top-level app type
newtype Fission a = Fission { unFission :: RIO Config a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader Config
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   )

instance MonadLogger Fission where
  monadLoggerLog loc src lvl msg = Fission $ monadLoggerLog loc src lvl msg

instance MonadTime Fission where
  currentTime = liftIO getCurrentTime

instance MonadReflectiveServer Fission where
  getHost = asks host

instance MonadDB (Transaction Fission) Fission where
  runDB transaction = do
    pool <- asks dbPool
    SQL.runSqlPool transaction pool

instance MonadAWS Fission where
  liftAWS awsAction = do
    accessKey <- asks awsAccessKey
    secretKey <- asks awsSecretKey
    env       <- newEnv $ FromKeys accessKey secretKey
   
    runResourceT $ runAWS env awsAction

instance MonadRoute53 Fission where
  update recordType url contents = do
    AWS.Route53MockEnabled mockRoute53 <- asks awsRoute53MockEnabled

    if mockRoute53
      then
         changeRecordMock
        
      else do
        logDebug $ "Updating DNS record at: " <> displayShow url
 
        req <- createChangeRequest

        AWS.within NorthVirginia do
          res <- send req
          return $ validate res

    where
      -- | Create the AWS change request for Route53
      createChangeRequest = do
        ZoneID zoneId <- asks awsZoneID
       
        let
          urlTxt = textDisplay url
          toSet  = addValues (resourceRecordSet urlTxt recordType) contents
          batch  = changeBatch . pure $ change Upsert toSet

        return $ changeResourceRecordSets (ResourceId zoneId) batch

      addValues :: ResourceRecordSet -> NonEmpty Text -> ResourceRecordSet
      addValues recordSet values =
        recordSet
          |> rrsTTL ?~ 10
          |> rrsResourceRecords ?~ (resourceRecord <$> values)

      changeRecordMock = do
          mockTime <- currentTime

          let
            mockMessage = mconcat
              [ "MOCK: Updating DNS "
              , show recordType
              , " record at: "
              , Text.unpack (textDisplay url)
              , " with "
              , show contents
              ]

            mockId             = "test123"
            mockChangeInfo     = changeInfo mockId Pending mockTime
            mockRecordResponse = changeResourceRecordSetsResponse 300 mockChangeInfo

          logDebug mockMessage
          return (Right mockRecordResponse)

instance MonadDNSLink Fission where
  set url@URL {..} (IPFS.CID hash) = do
    IPFS.Gateway gateway <- asks ipfsGateway

    let
      dnsLinkURL = URL.prefix' (URL.Subdomain "_dnslink") url
      dnsLink    = "dnslink=/ipfs/" <> hash

    AWS.update Cname url (pure gateway) >>= \case
      Left err ->
        return $ Left err

      Right _ ->
        AWS.update Txt dnsLinkURL (pure $ "\"" <> dnsLink <> "\"") <&> \case
          Left err -> Left  err
          Right _  -> Right url

  follow url@URL {..} URL { domainName = driveDomain, subdomain = driveSub } = do
    IPFS.Gateway gateway <- asks ipfsGateway

    let
      -- baseURL    = URL.normalizePrefix domainName subdomain
      -- sub        = maybe "" (\(URL.Subdomain txt) -> "." <> txt) subdomain
      -- dnsLinkURL = URL.prefix baseURL (URL.Subdomain $ "_dnslink" <> sub)
      dnsLinkURL = URL
        { domainName
        , subdomain = Just (URL.Subdomain "_dnslink") <> subdomain
        }
   
      URL.DomainName driveURL = URL.normalizePrefix driveDomain driveSub
      dnsLink                 = "dnslink=/ipns/" <> driveURL

    AWS.update Cname url (pure gateway) >>= \case
      Left err ->
        return $ Left err

      Right _ ->
        AWS.update Txt dnsLinkURL (pure $ "\"" <> dnsLink <> "\"") <&> \case
          Left err -> Left err
          Right _  -> Right ()

instance MonadLinkedIPFS Fission where
  getLinkedPeers = pure <$> asks ipfsRemotePeer

instance IPFS.MonadLocalIPFS Fission where
  runLocal opts arg = do
    IPFS.BinPath ipfs <- asks ipfsPath
    IPFS.Timeout secs <- asks ipfsTimeout

    let
      opts' = ("--timeout=" <> show secs <> "s") : opts
      args' = byteStringInput arg

    IPFS.runProc readProcess ipfs args' byteStringOutput opts' <&> \case
      (ExitSuccess, contents, _) ->
        Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            Left $ IPFS.Process.Timeout secs
 
        | otherwise ->
            Left $ IPFS.Process.UnknownErr stdErr

instance IPFS.MonadRemoteIPFS Fission where
  runRemote query = do
    peerID       <- asks ipfsRemotePeer
    IPFS.URL url <- asks ipfsURL
    manager      <- asks httpManager
    _            <- Peer.connectRetry peerID 2

    liftIO . runClientM query $ mkClientEnv manager url

instance MonadBasicAuth Heroku.Auth Fission where
  getVerifier = do
    Heroku.ID       hkuID   <- asks herokuID
    Heroku.Password hkuPass <- asks herokuPassword
    return $ Heroku.Auth <$> Auth.basic hkuID hkuPass

instance MonadAuth DID Fission where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unFission $ Auth.DID.handler req

instance MonadAuth Authorization Fission where
  getVerifier = do
    cfg <- ask
    return $ mkAuthHandler \req ->
      toHandler (runRIO cfg) . unFission $ Auth.Token.handler req

instance App.Domain.Initializer Fission where
  initial = asks baseAppDomainName

instance App.Content.Initializer Fission where
  placeholder = asks appPlaceholder

instance JWT.Resolver Fission where
  resolve cid@(IPFS.CID hash) =
    IPFS.runLocal ["cat"] (Lazy.fromStrict $ encodeUtf8 hash) <&> \case
      Left errMsg ->
        Left $ CannotResolve cid errMsg

      Right (Lazy.toStrict -> resolvedBS) ->
        case eitherDecodeStrict resolvedBS of
          Left  _   -> Left $ InvalidJWT resolvedBS
          Right jwt -> Right (decodeUtf8Lenient resolvedBS, jwt)

instance ServerDID Fission where
  getServerDID = asks fissionDID

instance PublicizeServerDID Fission where
  publicize = do
    AWS.Route53MockEnabled mockRoute53 <- asks awsRoute53MockEnabled
 
    Host host <- Reflective.getHost
    did       <- getServerDID

    let
      ourURL         = URL (URL.DomainName . Text.pack $ baseUrlHost host) Nothing
      txtRecordURL   = URL.prefix' (URL.Subdomain "_did") ourURL
      txtRecordValue = decodeUtf8Lenient . Lazy.toStrict $ JSON.encode did

    if mockRoute53
      then do
        logInfo $ mconcat
          [ "MOCK: Setting DNS setting "
          , textDisplay txtRecordURL
          , " to "
          , txtRecordValue
          ]
         
        return ok
       
      else
        AWS.update Txt txtRecordURL (pure txtRecordValue) <&> \case
          Left err ->
            Left err

          Right resp ->
            let
              status = view crrsrsResponseStatus resp
            in
              if status < 300
                then ok
                else Left $ Web.Error.toServerError status

instance User.Creator Fission where
  create username@(Username rawUN) pk email now =
    runDB (User.create username pk email now) >>= \case
      Left err ->
        return $ Left err

      Right userId ->
        User.updatePublicKey userId pk now >>= \case
          Left err ->
            return $ Error.relaxedLeft err

          Right _ -> do
            domainName <- asks baseUserDataRootDomain
            driveURL   <- asks liveDriveURL

            let subdomain = Just $ Subdomain rawUN

            DNSLink.follow URL {..} driveURL <&> \case
              Left serverError -> Error.openLeft serverError
              Right _          -> Right userId

  createWithHeroku herokuUUID herokuRegion username password now =
    runDB $ User.createWithHeroku herokuUUID herokuRegion username password now

  createWithPassword username password email now =
    runDB (User.createWithPassword username password email now) >>= \case
      Left err ->
        return $ Left err

      Right userId ->
        App.createWithPlaceholder userId now <&> \case
          Left err -> Error.relaxedLeft err
          Right _  -> Right userId

instance User.Modifier Fission where
  updatePassword uID pass now =
    runDB $ User.updatePassword uID pass now

  updatePublicKey uID pk now =
    runDB (User.updatePublicKey uID pk now) >>= \case
      Left err ->
        return $ Left err

      Right _ -> do
        runDB (User.getById uID) >>= \case
          Nothing -> 
            return . Error.openLeft $ NotFound @User

          Just (Entity _ User { userUsername = Username rawUN }) -> do
            domainName <- asks baseUserDataRootDomain

            let
              subdomain = Just $ Subdomain rawUN
              url = URL {domainName, subdomain = Just (Subdomain "_did") <> subdomain}
              did = textDisplay (DID pk Key) -- FIXME break up anythunbg > 255 chars

            AWS.update Txt url (pure did) <&> \case
              Left  serverErr -> Error.openLeft serverErr
              Right _         -> Right pk

  setData userId newCID now = do
    runDB (User.setData userId newCID now) >>= \case
      Left err ->
        return $ Left err
       
      Right () -> do
        runDB (User.getById userId) >>= \case
          Nothing ->
            return . Error.openLeft $ NotFound @User
           
          Just (Entity _ User { userUsername = Username username }) -> do
            userDataDomain <- asks baseUserDataRootDomain
           
            let
              url = URL
                { domainName = userDataDomain
                , subdomain  = Just . Subdomain $ "_dnslink.files." <> username
                }

            DNSLink.set url newCID <&> \case
              Left err -> Error.openLeft err
              Right _  -> ok

instance App.Retriever Fission where
  byId uId appId = runDB $ App.byId uId appId
  ownedBy uId    = runDB $ App.ownedBy uId

instance App.Creator Fission where
  create ownerID cid now =
    runDB (App.create ownerID cid now) >>= \case
      Left err ->
        return $ Left err

      Right (appId, subdomain) -> do
        appCID     <- App.Content.placeholder
        domainName <- App.Domain.initial
 
        let
          url = URL { domainName, subdomain = Just subdomain }

        DNSLink.set url appCID <&> \case
          Left  err -> Error.openLeft err
          Right _   -> Right (appId, subdomain)

instance App.Modifier Fission where
  updateCID userId appId newCID now =
    App.byId userId appId >>= \case
      Left err ->
        return $ Error.relaxedLeft err

      Right (Entity _ app) -> do
        if isOwnedBy userId app
          then
            runDB (App.updateCID userId appId newCID now) >>= \case
              Left err ->
                return $ Left err
               
              Right () -> do
                appDomains <- runDB $ App.Domain.allForApp appId
                forM_ appDomains \(Entity _ AppDomain {..}) ->
                  let
                    url = URL
                      { domainName = appDomainDomainName
                      , subdomain  = appDomainSubdomain
                      }
                  in
                    DNSLink.set url newCID

                return ok
           
          else
            return . Error.openLeft $ ActionNotAuthorized @App userId

instance Heroku.AddOn.Creator Fission where
  create uuid region now = runDB $ Heroku.AddOn.create uuid region now

