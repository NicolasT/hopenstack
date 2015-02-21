{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.OpenStack.Client.Wreq (
      runOperation
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Aeson
import qualified Network.Wreq as W

import Network.OpenStack.Client

runOperation :: (ToJSON a, FromJSON b) => Handler m t a IO b
runOperation op = case op of
    OGETAnonymous _ _ -> runGETAnonymous op
    OPOSTAnonymous _ _ -> runPOSTAnonymous op
    OPUTAnonymous _ _ -> runPUTAnonymous op
    ODELETEAnonymous _ -> runDELETEAnonymous op

runGETAnonymous :: FromJSON b => Handler 'GET 'Anonymous () IO b
runGETAnonymous op@(OGETAnonymous processOptions processResponse) url = do
    let full = url ++ operationSuffix op
    processResponse <$> (W.asJSON =<< W.getWith (processOptions W.defaults) full)

runPOSTAnonymous :: (ToJSON a, FromJSON b) => Handler 'POST 'Anonymous a IO b
runPOSTAnonymous op@(OPOSTAnonymous processOptions processResponse) url a = do
    let full = url ++ operationSuffix op
    processResponse <$> (W.asJSON =<< W.postWith (processOptions W.defaults) full (toJSON a))

runPUTAnonymous :: (ToJSON a, FromJSON b) => Handler 'PUT 'Anonymous a IO b
runPUTAnonymous op@(OPUTAnonymous processOptions processResponse) url a = do
    let full = url ++ operationSuffix op
    processResponse <$> (W.asJSON =<< W.putWith (processOptions W.defaults) full (toJSON a))

runDELETEAnonymous :: Handler 'DELETE 'Anonymous () IO ()
runDELETEAnonymous op@(ODELETEAnonymous processOptions) url = do
    let full = url ++ operationSuffix op
    void $ W.deleteWith (processOptions W.defaults) full
