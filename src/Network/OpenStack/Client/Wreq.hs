{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.OpenStack.Client.Wreq (
      runOperation
    ) where

import Data.Aeson
import qualified Network.Wreq as W

import Network.OpenStack.Client

runOperation :: ToJSON a => Handler m t a IO b
runOperation op = case op of
    OGETAnonymous _ _ -> runGETAnonymous op
    OPOSTAnonymous _ _ -> runPOSTAnonymous op
    OPUTAnonymous _ _ -> runPUTAnonymous op
    ODELETEAnonymous _ _ -> runDELETEAnonymous op

runGETAnonymous :: Handler 'GET 'Anonymous () IO b
runGETAnonymous op@(OGETAnonymous processOptions processResponse) url = do
    let full = url ++ operationSuffix op
    processResponse =<< W.getWith (processOptions W.defaults) full

runPOSTAnonymous :: ToJSON a => Handler 'POST 'Anonymous a IO b
runPOSTAnonymous op@(OPOSTAnonymous processOptions processResponse) url a = do
    let full = url ++ operationSuffix op
    processResponse =<< W.postWith (processOptions W.defaults) full (toJSON a)

runPUTAnonymous :: ToJSON a => Handler 'PUT 'Anonymous a IO b
runPUTAnonymous op@(OPUTAnonymous processOptions processResponse) url a = do
    let full = url ++ operationSuffix op
    processResponse =<< W.putWith (processOptions W.defaults) full (toJSON a)

runDELETEAnonymous :: Handler 'DELETE 'Anonymous () IO ()
runDELETEAnonymous op@(ODELETEAnonymous processOptions processResponse) url = do
    let full = url ++ operationSuffix op
    processResponse =<< W.deleteWith (processOptions W.defaults) full
