{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OpenStack.Keystone (
      DefaultScopeTokenRequest(..)
    , DomainScopeTokenRequest(..)
    , createToken
    , defaultScope
    , domainScope
    , Token, getToken
    ) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch (throwM)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable
import qualified Network.Wreq as W

import Network.OpenStack.Client

data ScopeIdentifier = DefaultScope | DomainScope
    deriving (Show, Eq, Typeable)

data Scope (s :: ScopeIdentifier) where
    Scope :: Scope s

deriving instance Show (Scope s)
deriving instance Eq (Scope s)
deriving instance Typeable Scope

defaultScope :: Scope DefaultScope
defaultScope = Scope

domainScope :: Scope DomainScope
domainScope = Scope

data DefaultScopeTokenRequest = DefaultScopeTokenRequest { defaultScopeTokenRequestName :: Text
                                                         , defaultScopeTokenRequestPassword :: Text
                                                         }
    deriving (Show, Eq, Typeable)

instance ToJSON DefaultScopeTokenRequest where
    toJSON request =
        object [
            "auth" .= object [
                "identity" .= object [
                    "methods" .= [ "password" :: Text ],
                    "password" .= object [
                        "user" .= object [
                            "domain" .= object [
                                "id" .= ("default" :: Text) ],
                            "name" .= defaultScopeTokenRequestName request,
                            "password" .= defaultScopeTokenRequestPassword request
        ]]]]]


data DomainScopeTokenRequest = DomainScopeTokenRequest Text Text Text
    deriving (Show, Eq, Typeable)


type family TokenRequest (s :: ScopeIdentifier) where
    TokenRequest DefaultScope = DefaultScopeTokenRequest
    TokenRequest DomainScope = DomainScopeTokenRequest

newtype Token (s :: ScopeIdentifier) = Token { getToken :: ByteString }
    deriving (Show, Eq, Typeable)

createToken :: Scope s -> POST "/auth/tokens" Anonymous (TokenRequest s) (Token s)
createToken _ = operation' id getToken'
  where
    header :: IsString a => a
    header = "X-Subject-Token"
    getToken' resp = case resp ^? W.responseHeader header of
        Nothing -> throwM $ ResponseProcessingError $ "Missing '" ++ header ++ "' header"
        Just bs -> return $ Token bs
