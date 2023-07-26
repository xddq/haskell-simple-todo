{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read (decimal)
import Database (CreateTodoInput (CreateTodoInput), Todo (Todo), UpdateTodoInput (UpdateTodoInput, updateTodoInputId), createTodo, deleteTodo, getTodoById, getTodos, updateTodoById)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (ActionM, body, delete, get, json, middleware, param, patch, post, scottyApp, setHeader, status, text)

-- Client facing errors. Whenever we get an error, we return additional
-- information in this format.
newtype ApiError = ApiError {apiErrorMessage :: Text}
  deriving (Show)

instance ToJSON ApiError where
  toJSON (ApiError msg) = object ["message" .= msg]

mkApiError :: Text -> ApiError
mkApiError = ApiError

-- How incoming JSON will be parsed to our internal CreateTodoInput type
instance FromJSON CreateTodoInput where
  parseJSON = withObject "CreateTodoInput" $ \obj -> do
    createTodoInputFromJsonText <- obj .: "text"
    createTodoInputFromJsonDone <- obj .: "done"
    return (CreateTodoInput createTodoInputFromJsonText createTodoInputFromJsonDone)

-- How incoming JSON will be parsed to our internal UpdateTodoInput type
instance FromJSON UpdateTodoInput where
  parseJSON = withObject "UpdateTodoInput" $ \obj -> do
    updateTodoInputFromJsonId <- obj .: "id"
    updateTodoInputFromJsonText <- obj .: "text"
    updateTodoInputFromJsonDone <- obj .: "done"
    return (UpdateTodoInput updateTodoInputFromJsonId updateTodoInputFromJsonText updateTodoInputFromJsonDone)

-- How internal Todo will be parsed to outgoing JSON
instance ToJSON Todo where
  toJSON (Todo todoToJsonId todoToJsontext todoToJsonDone) =
    object
      [ "id" .= todoToJsonId,
        "text" .= todoToJsontext,
        "done" .= todoToJsonDone
      ]

-- TODO/MAYBE: a) is there a 'Status' for all 400 and 500 codes?
-- b) create data type with all errors that we want to return and use that type
-- instead. (e.g. 400, 404, 500, ..?)
sendError :: ApiError -> Status -> ActionM ()
sendError x y = do
  status y
  json x

sendSuccess :: ToJSON a => a -> ActionM ()
sendSuccess x = do
  status status200
  json x

-- TODO: why is this signature not working?
-- allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PATCH", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev
    middleware allowCors

    get "/" $ do
      setHeader "Content-Type" "text/plain; charset=utf-8"
      status status200
      text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

    -- GET all todos
    get "/todos" $ do
      todos <- liftIO (getTodos conn)
      sendSuccess todos

    -- GET one todo
    get "/todos/:id" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> sendError (mkApiError $ pack err) status400
        Right (parsedId, _rest) -> do
          listWithTodo <- liftIO (getTodoById conn parsedId)
          case listToMaybe listWithTodo of
            Just todo -> sendSuccess todo
            Nothing -> sendError (mkApiError "not found") status404

    -- DELETE one todo
    delete "/todos/:id" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> sendError (mkApiError $ pack err) status400
        Right (parsedId, _rest) -> do
          deletedRowsCount <- liftIO (deleteTodo conn parsedId)
          if deletedRowsCount == 1
            then sendSuccess $ decodeUtf8 $ encode $ object ["message" .= ("ok" :: Text)]
            else sendError (mkApiError "not found") status404

    -- CREATE one todo
    post "/todos" $ do
      unparsedJson <- body
      case decode unparsedJson :: Maybe Database.CreateTodoInput of
        Just createTodoInput -> do
          listWithCreatedTodo <- liftIO (createTodo conn createTodoInput)
          case listToMaybe listWithCreatedTodo of
            Just createdTodo -> sendSuccess createdTodo
            Nothing -> sendError (mkApiError "should never happen. creating a todo returned an empty list") status500
        Nothing -> sendError (mkApiError "invalid input") status400

    -- UPDATE one todo
    patch "/todos/:id" $ do
      unparsedIdFromPath <- param "id"
      case decimal unparsedIdFromPath of
        Left err -> sendError (mkApiError $ pack err) status400
        Right (idOfToBeUpdatedTodo, _rest) -> do
          unparsedJson <- body
          case decode unparsedJson :: Maybe UpdateTodoInput of
            Just updateTodoInput -> do
              if idOfToBeUpdatedTodo /= updateTodoInputId updateTodoInput
                then sendError (mkApiError "id of payload and path did not match") status400
                else do
                  updatedTodo <- liftIO (updateTodoById conn updateTodoInput)
                  case listToMaybe updatedTodo of
                    Just todo -> sendSuccess todo
                    Nothing -> sendError (mkApiError "not found") status404
            Nothing -> sendError (mkApiError "invalid input") status400
