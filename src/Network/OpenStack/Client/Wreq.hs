{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.OpenStack.Client.Wreq (
      runOperation
    ) where

import Control.Lens hiding (op)
import Data.Aeson
import qualified Network.Wreq as W

import Network.OpenStack.Client

runOperation :: (ToJSON a, FromJSON b) => Handler m a IO b
runOperation op = case op of
    OGET -> runGET op
    OPOST -> runPOST op
    OPUT -> runPUT op
    ODELETE -> runDELETE op

runGET :: FromJSON b => Handler 'GET () IO b
runGET op url = do
    let full = url ++ operationSuffix op
    resp <- W.asJSON =<< W.get full
    return $ resp ^. W.responseBody

runPOST :: (ToJSON a, FromJSON b) => Handler 'POST a IO b
runPOST op url a = do
    let full = url ++ operationSuffix op
    resp <- W.asJSON =<< W.post full (toJSON a)
    return $ resp ^. W.responseBody

runPUT :: (ToJSON a, FromJSON b) => Handler 'PUT a IO b
runPUT op url a = do
    let full = url ++ operationSuffix op
    resp <- W.asJSON =<< W.put full (toJSON a)
    return $ resp ^. W.responseBody

runDELETE :: Handler 'DELETE () IO ()
runDELETE op url = do
    let full = url ++ operationSuffix op
    _ <- W.delete full
    return ()
