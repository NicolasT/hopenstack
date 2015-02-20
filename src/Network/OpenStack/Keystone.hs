{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OpenStack.Keystone (
      DefaultRequest(..)
    , DomainRequest(..)
    , createToken
    , defaultScope
    , domainScope
    , Token, unToken
    ) where

import Data.Aeson
import Data.Text (Text)
import Data.Typeable

import Network.OpenStack.Client

data ScopeIdentifier = Default | Domain
    deriving (Show, Eq, Typeable)

data Scope (s :: ScopeIdentifier) where
    Scope :: Scope s

deriving instance Show (Scope s)
deriving instance Eq (Scope s)
deriving instance Typeable Scope

defaultScope :: Scope Default
defaultScope = Scope

domainScope :: Scope Domain
domainScope = Scope

data DefaultRequest = DefaultRequest String String
    deriving (Show, Eq, Typeable)

data DomainRequest = DomainRequest Text Text Text
    deriving (Show, Eq, Typeable)

instance ToJSON DomainRequest where
    toJSON (DomainRequest name password domain) = object [ "name" .= name
                                                         , "password" .= password
                                                         , "domain" .= domain
                                                         ]

type family TokenRequest (s :: ScopeIdentifier) where
    TokenRequest Default = DefaultRequest
    TokenRequest Domain = DomainRequest

newtype Token (s :: ScopeIdentifier) = Token { unToken :: String }
    deriving (Show, Eq, Typeable)

instance FromJSON (Token s) where
    parseJSON _ = return (Token "hello")

createToken :: Scope s -> POST "/auth/tokens" (TokenRequest s) (Token s)
createToken _ = operation
