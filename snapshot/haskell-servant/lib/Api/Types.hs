{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Swagger Petstore.Types (
  Order (..),
  Category (..),
  User (..),
  Tag (..),
  Pet (..),
  ApiResponse (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))
import IO.OpenAPI.Model.Default


-- | 
 Order = Order
  { id :: int -- ^ 
  , petId :: int -- ^ 
  , quantity :: int -- ^ 
  , shipDate :: string -- ^ 
  , status :: string -- ^ Order Status
  , complete :: bool -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON Order where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- | 
 Category = Category
  { id :: int -- ^ 
  , name :: string -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Category where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON Category where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- | 
 User = User
  { id :: int -- ^ 
  , username :: string -- ^ 
  , firstName :: string -- ^ 
  , lastName :: string -- ^ 
  , email :: string -- ^ 
  , password :: string -- ^ 
  , phone :: string -- ^ 
  , userStatus :: int -- ^ User Status
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- | 
 Tag = Tag
  { id :: int -- ^ 
  , name :: string -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON Tag where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- | 
 Pet = Pet
  { id :: int -- ^ 
  , category :: struct{} -- ^ 
  , name :: string -- ^ 
  , photoUrls :: [100]string -- ^ 
  , tags :: [100]struct{} -- ^ 
  , status :: string -- ^ pet status in the store
  } deriving (Show, Eq, Generic)

instance FromJSON Pet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON Pet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- | 
 ApiResponse = ApiResponse
  { code :: int -- ^ 
  , Type :: string -- ^ 
  , message :: string -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON ApiResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "")
instance ToJSON ApiResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ 
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
