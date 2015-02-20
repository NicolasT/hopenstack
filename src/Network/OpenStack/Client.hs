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
    , Operation(..)
    , OperationCall
    , GET, POST, PUT, DELETE
    , URL
    , Handler
    , operation
    , operationSuffix
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data Method = GET | POST | PUT | DELETE
    deriving (Show, Eq, Typeable)

data Operation (m :: Method) (s :: Symbol) a b where
    OGET :: Operation 'GET s () b
    OPOST :: Operation 'POST s a b
    OPUT :: Operation 'PUT s a b
    ODELETE :: Operation 'DELETE s () ()

deriving instance Show (Operation m s a b)
deriving instance Eq (Operation m s a b)
deriving instance Typeable Operation

type GET s b = Operation 'GET s () b
type POST s a b = Operation 'POST s a b
type PUT s a b = Operation 'PUT s a b
type DELETE s = Operation 'DELETE s () ()

class OperationClass (m :: Method) (s :: Symbol) a b where
    operation :: Operation m s a b

instance OperationClass 'GET s () b where
    operation = OGET

instance OperationClass 'POST s a b where
    operation = OPOST

instance OperationClass 'PUT s a b where
    operation = OPUT

instance OperationClass 'DELETE s () () where
    operation = ODELETE

type URL = String

type family OperationCall (h :: Method) a (m :: * -> *) b where
    OperationCall 'GET () m b = URL -> m b
    OperationCall 'POST a m b = URL -> a -> m b
    OperationCall 'PUT a m b = URL -> a -> m b
    OperationCall 'DELETE () m () = URL -> m ()


operationSuffix :: forall m s a b. KnownSymbol s => Operation m s a b -> String
operationSuffix _ = symbolVal (Proxy :: Proxy s)


type Handler h a m b = KnownSymbol s => Operation h s a b -> OperationCall h a m b
