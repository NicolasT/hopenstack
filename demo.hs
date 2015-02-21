{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import System.Environment

import Network.OpenStack.Client.Wreq
import Network.OpenStack.Keystone

main :: IO ()
main = do
    [url, user, pass] <- getArgs
    token <- runOperation (createToken defaultScope) url (DefaultScopeTokenRequest (pack user) (pack pass))
    print token
