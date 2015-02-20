{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.OpenStack.Client.Wreq (
      runOperation
    ) where

import Data.Aeson

import Network.OpenStack.Client

runOperation :: (ToJSON a, FromJSON b) => Handler m a IO b
runOperation op = case op of
    OGET -> runGET op
    OPOST -> runPOST op
    OPUT -> runPUT op
    ODELETE -> runDELETE op

runGET :: Handler 'GET () IO b
runGET op url = do
    let full = url ++ operationSuffix op
    putStrLn full
    return undefined

runPOST :: (ToJSON a, FromJSON b) => Handler 'POST a IO b
runPOST op url value = do
    let full = url ++ operationSuffix op
    putStrLn $ "POST " ++ full
    print $ encode value
    putStrLn ""
    case eitherDecode "[1, 2, 3]" of
        Left e -> error e
        Right (v :: b) -> return v

runPUT :: Handler 'PUT a IO b
runPUT = undefined

runDELETE :: Handler 'DELETE () IO ()
runDELETE = undefined
