{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Swagger Petstore.API
  -- * Client and Server
  ( ServerConfig(..)
  , Swagger PetstoreBackend
  , createSwagger PetstoreClient
  , runSwagger PetstoreServer
  , runSwagger PetstoreClient
  , runSwagger PetstoreClientWithManager
  , Swagger PetstoreClient
  -- ** Servant
  , Swagger PetstoreAPI
  ) where

import Swagger Petstore.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for Swagger Petstore.
type Swagger PetstoreAPI
    =     -- 'addPet' route
    :<|>  -- 'updatePet' route
    :<|>  -- 'findPetsByStatus' route
    :<|>  -- 'findPetsByTags' route
    :<|>  -- 'getPetById' route
    :<|>  -- 'updatePetWithForm' route
    :<|>  -- 'deletePet' route
    :<|>  -- 'uploadFile' route
    :<|>  -- 'getInventory' route
    :<|>  -- 'placeOrder' route
    :<|>  -- 'getOrderById' route
    :<|>  -- 'deleteOrder' route
    :<|>  -- 'createUser' route
    :<|>  -- 'createUsersWithArrayInput' route
    :<|>  -- 'createUsersWithListInput' route
    :<|>  -- 'loginUser' route
    :<|>  -- 'logoutUser' route
    :<|>  -- 'getUserByName' route
    :<|>  -- 'updateUser' route
    :<|>  -- 'deleteUser' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for Swagger Petstore.
-- The backend can be used both for the client and the server. The client generated from the Swagger Petstore Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createSwagger PetstoreClient@). Alternatively, provided
-- a backend, the API can be served using @runSwagger PetstoreServer@.
data Swagger PetstoreBackend m = Swagger PetstoreBackend
  { addPet :: {- ^  -}
  , updatePet :: {- ^  -}
  , findPetsByStatus :: {- ^ Multiple status values can be provided with comma separated strings -}
  , findPetsByTags :: {- ^ Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. -}
  , getPetById :: {- ^ Returns a single pet -}
  , updatePetWithForm :: {- ^  -}
  , deletePet :: {- ^  -}
  , uploadFile :: {- ^  -}
  , getInventory :: {- ^ Returns a map of status codes to quantities -}
  , placeOrder :: {- ^  -}
  , getOrderById :: {- ^ For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions -}
  , deleteOrder :: {- ^ For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors -}
  , createUser :: {- ^ This can only be done by the logged in user. -}
  , createUsersWithArrayInput :: {- ^  -}
  , createUsersWithListInput :: {- ^  -}
  , loginUser :: {- ^  -}
  , logoutUser :: {- ^  -}
  , getUserByName :: {- ^  -}
  , updateUser :: {- ^ This can only be done by the logged in user. -}
  , deleteUser :: {- ^ This can only be done by the logged in user. -}
  }

newtype Swagger PetstoreClient a = Swagger PetstoreClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative Swagger PetstoreClient where
  pure x = Swagger PetstoreClient (\_ _ -> pure x)
  (Swagger PetstoreClient f) <*> (Swagger PetstoreClient x) =
    Swagger PetstoreClient (\manager url -> f manager url <*> x manager url)

instance Monad Swagger PetstoreClient where
  (Swagger PetstoreClient a) >>= f =
    Swagger PetstoreClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO Swagger PetstoreClient where
  liftIO io = Swagger PetstoreClient (\_ _ -> liftIO io)

createSwagger PetstoreClient :: Swagger PetstoreBackend Swagger PetstoreClient
createSwagger PetstoreClient = Swagger PetstoreBackend{..}
  where
    ((coerce -> addPet) :<|>
     (coerce -> updatePet) :<|>
     (coerce -> findPetsByStatus) :<|>
     (coerce -> findPetsByTags) :<|>
     (coerce -> getPetById) :<|>
     (coerce -> updatePetWithForm) :<|>
     (coerce -> deletePet) :<|>
     (coerce -> uploadFile) :<|>
     (coerce -> getInventory) :<|>
     (coerce -> placeOrder) :<|>
     (coerce -> getOrderById) :<|>
     (coerce -> deleteOrder) :<|>
     (coerce -> createUser) :<|>
     (coerce -> createUsersWithArrayInput) :<|>
     (coerce -> createUsersWithListInput) :<|>
     (coerce -> loginUser) :<|>
     (coerce -> logoutUser) :<|>
     (coerce -> getUserByName) :<|>
     (coerce -> updateUser) :<|>
     (coerce -> deleteUser)) = client (Proxy :: Proxy Swagger PetstoreAPI)

-- | Run requests in the Swagger PetstoreClient monad.
runSwagger PetstoreClient :: ServerConfig -> Swagger PetstoreClient a -> ExceptT ServantError IO a
runSwagger PetstoreClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runSwagger PetstoreClientWithManager manager clientConfig cl

-- | Run requests in the Swagger PetstoreClient monad using a custom manager.
runSwagger PetstoreClientWithManager :: Manager -> ServerConfig -> Swagger PetstoreClient a -> ExceptT ServantError IO a
runSwagger PetstoreClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the Swagger Petstore server at the provided host and port.
runSwagger PetstoreServer :: MonadIO m => ServerConfig -> Swagger PetstoreBackend (ExceptT ServantErr IO)  -> m ()
runSwagger PetstoreServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy Swagger PetstoreAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend Swagger PetstoreBackend{..} =
      (coerce addPet :<|>
       coerce updatePet :<|>
       coerce findPetsByStatus :<|>
       coerce findPetsByTags :<|>
       coerce getPetById :<|>
       coerce updatePetWithForm :<|>
       coerce deletePet :<|>
       coerce uploadFile :<|>
       coerce getInventory :<|>
       coerce placeOrder :<|>
       coerce getOrderById :<|>
       coerce deleteOrder :<|>
       coerce createUser :<|>
       coerce createUsersWithArrayInput :<|>
       coerce createUsersWithListInput :<|>
       coerce loginUser :<|>
       coerce logoutUser :<|>
       coerce getUserByName :<|>
       coerce updateUser :<|>
       coerce deleteUser)
