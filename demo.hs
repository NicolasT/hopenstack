{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text ()

import Network.OpenStack.Client.Wreq
import Network.OpenStack.Keystone

main :: IO ()
main = do
    token <- runOperation (createToken domainScope) service request
    print token
  where
    service = "http://localhost:5000/v3"
    request = DomainRequest "nicolas" "pass" "mydomain"
