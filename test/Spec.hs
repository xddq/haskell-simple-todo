{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.PostgreSQL.Simple
import Network.Wai (Application)
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Wai (get, shouldRespondWith, with, (<:>))
import Test.Hspec.Wai.Matcher (ResponseMatcher (matchHeaders))
import Webapp (mkApp)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {connectHost = "localhost", connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}
  hspec $ spec $ mkApp conn

-- testing
spec :: IO Application -> Spec
spec application = with application $ do
  -- test the "dummy" route
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with 'Welcome to your todo list! You might want to query /todos instead :]'" $ do
      get "/" `shouldRespondWith` "Welcome to your todo list! You might want to query /todos instead :]"

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  -- TODO: perhaps run integration tests with database against crud routes
  describe "GET /todos" $ do
    it "responds with 200" $ do
      get "/todos" `shouldRespondWith` 200
