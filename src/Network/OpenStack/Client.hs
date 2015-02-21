{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OpenStack.Client (
      Method(..)
    , Authentication(..)
    , Operation(..)
    , OperationCall
    , GET, POST, PUT, DELETE
    , URL
    , Handler
    , operation'
    , operation
    , operationSuffix
    ) where

import Control.Lens
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.Wreq as W

data Method = GET | POST | PUT | DELETE
    deriving (Show, Eq, Typeable)

data Authentication = Anonymous | Authenticated
    deriving (Show, Eq, Typeable)

type OptionsProcessor = W.Options -> W.Options
type ResponseProcessor b = W.Response b -> b

data Operation (m :: Method) (s :: Symbol) (t :: Authentication) a b where
    OGETAnonymous :: OptionsProcessor -> ResponseProcessor b -> Operation 'GET s 'Anonymous () b
    OPOSTAnonymous :: OptionsProcessor -> ResponseProcessor b -> Operation 'POST s 'Anonymous a b
    OPUTAnonymous :: OptionsProcessor -> ResponseProcessor b -> Operation 'PUT s 'Anonymous a b
    ODELETEAnonymous :: OptionsProcessor -> Operation 'DELETE s 'Anonymous () ()

deriving instance Typeable Operation

type GET s t b = Operation 'GET s t () b
type POST s t a b = Operation 'POST s t a b
type PUT s t a b = Operation 'PUT s t a b
type DELETE s t = Operation 'DELETE s t () ()

class OperationClass (m :: Method) (s :: Symbol) (t :: Authentication) a b where
    operation' :: OptionsProcessor -> ResponseProcessor b -> Operation m s t a b

    operation :: Operation m s t a b
    operation = operation' id (\resp -> resp ^. W.responseBody)

instance OperationClass 'GET s 'Anonymous () b where
    operation' = OGETAnonymous

instance OperationClass 'POST s 'Anonymous a b where
    operation' = OPOSTAnonymous

instance OperationClass 'PUT s 'Anonymous a b where
    operation' = OPUTAnonymous

instance OperationClass 'DELETE s 'Anonymous () () where
    operation' o _ = ODELETEAnonymous o
    operation = operation' id (const ())


type URL = String

type family OperationCall (h :: Method) (t :: Authentication) a (m :: * -> *) b where
    OperationCall 'GET 'Anonymous () m b = URL -> m b
    OperationCall 'POST 'Anonymous a m b = URL -> a -> m b
    OperationCall 'PUT 'Anonymous a m b = URL -> a -> m b
    OperationCall 'DELETE 'Anonymous () m () = URL -> m ()

operationSuffix :: forall m s t a b. KnownSymbol s => Operation m s t a b -> String
operationSuffix _ = symbolVal (Proxy :: Proxy s)


type Handler h t a m b = KnownSymbol s => Operation h s t a b -> OperationCall h t a m b
