{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp (run)
import Webapp (mkApp)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {connectHost = "localhost", connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}
  waiApp <- mkApp conn
  putStrLn "Running app on localhost:3000"
  run 3000 waiApp
