{-# LANGUAGE DeriveGeneric #-}
-- seems to be "desctructuring" from js/ts
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Reader
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode, object, withObject, (.:), (.=))
import Data.Int
import Data.Maybe (listToMaybe)
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Prelude.Compat
import Test.Hspec
import qualified Test.Hspec.Wai as H
import Web.Scotty
import Prelude ()

-- Client facing errors. Whenever we get an error, we return additional
-- information in this format.
newtype ApiError = ApiError {apiErrrorMessage :: Text}
  deriving (Show, Generic)

instance ToJSON ApiError where
  toJSON (ApiError msg) = object ["message" .= msg]

-- for validating the todo we pass when creating a new todo
data CreateTodoInput = CreateTodoInput
  { createTodoInputText :: String,
    createTodoInputDone :: Bool
  }
  deriving (Show, Generic)

instance ToRow CreateTodoInput where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow CreateTodoInput {createTodoInputDone, createTodoInputText} = toRow (createTodoInputDone, createTodoInputText)

instance FromJSON CreateTodoInput where
  parseJSON = withObject "CreateTodoInput" $ \obj -> do
    createTodoInputFromJsonText <- obj .: "text"
    createTodoInputFromJsonDone <- obj .: "done"
    return (CreateTodoInput createTodoInputFromJsonText createTodoInputFromJsonDone)

-- for validating the todo we pass when updating a todo
data UpdateTodoInput = UpdateTodoInput
  { updateTodoInputId :: Int,
    updateTodoInputText :: String,
    updateTodoInputDone :: Bool
  }
  deriving (Show, Generic)

instance ToRow UpdateTodoInput where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow UpdateTodoInput {updateTodoInputText, updateTodoInputDone, updateTodoInputId} = toRow (updateTodoInputText, updateTodoInputDone, updateTodoInputId)

instance FromJSON UpdateTodoInput where
  parseJSON = withObject "CreateTodoInput" $ \obj -> do
    updateTodoInputFromJsonId <- obj .: "id"
    updateTodoInputFromJsonText <- obj .: "text"
    updateTodoInputFromJsonDone <- obj .: "done"
    return (UpdateTodoInput updateTodoInputFromJsonText updateTodoInputFromJsonDone updateTodoInputFromJsonId)

sendError :: Text -> Status -> ActionM ()
sendError message responseStatus = do
  setHeader "Content-Type" "application/json"
  status responseStatus
  text $ decodeUtf8 $ encode $ ApiError message

sendSuccess :: Text -> ActionM ()
sendSuccess message = do
  setHeader "Content-Type" "application/json"
  status status200
  text message

data Todo = Todo
  { todoId :: Int,
    todoText :: Text,
    todoDone :: Bool
  }
  deriving (Show, Generic)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

instance FromJSON Todo

instance ToJSON Todo where
  toJSON (Todo todoToJsonId todoToJsontext todoToJsonDone) =
    object
      [ "id" .= todoToJsonId,
        "text" .= todoToJsontext,
        "done" .= todoToJsonDone
      ]

createTodo :: Connection -> CreateTodoInput -> IO [Todo]
createTodo conn = query conn "INSERT INTO todos (done,text) VALUES (?,?) RETURNING *"

getTodos :: Connection -> IO [Todo]
getTodos conn = query_ conn "SELECT * FROM todos"

getTodoById :: Connection -> Int -> IO [Todo]
getTodoById conn idOfTodo = query conn "SELECT * FROM todos WHERE id = ?" [idOfTodo]

-- TODO: why does this one not work??
-- getTodoById conn idOfTodo = query conn "SELECT (id,text,done) FROM todos WHERE id = ?" [idOfTodo]

updateTodoById :: Connection -> UpdateTodoInput -> IO [Todo]
updateTodoById conn = query conn "UPDATE todos SET text=?,done=? WHERE id=? RETURNING *"

deleteTodo :: Connection -> Int -> IO Int64
deleteTodo conn todoId = execute conn "DELETE FROM todos WHERE id = ?" (Only todoId)

-- TODO: why is this signature not working?
-- allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PATCH", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {connectHost = "localhost", connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}
  hspec $ spec $ app conn

-- app
app :: Connection -> IO Application
app conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev
    middleware allowCors

    get "/" $ do
      text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

    -- GET all todos
    get "/todos" $ do
      todos <- liftIO (getTodos conn)
      sendSuccess (decodeUtf8 $ encode todos)

    -- GET one todo
    get "/todos/:id" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> sendError (pack err) status400
        Right (parsedId, _rest) -> do
          listWithTodo <- liftIO (getTodoById conn parsedId)
          case listToMaybe listWithTodo of
            Just todo -> sendSuccess $ decodeUtf8 $ encode todo
            Nothing -> sendError "not found" status404

    -- DELETE one todo
    delete "/todos/:id" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> sendError (pack err) status400
        Right (parsedId, _rest) -> do
          deletedRowsCount <- liftIO (deleteTodo conn parsedId)
          if deletedRowsCount == 1 then sendSuccess "ok" else sendError "not found" status404

    -- CREATE one todo
    post "/todos" $ do
      unparsedJson <- body
      case decode unparsedJson :: Maybe CreateTodoInput of
        Just createTodoInput -> do
          listWithCreatedTodo <- liftIO (createTodo conn createTodoInput)
          case listToMaybe listWithCreatedTodo of
            Just createdTodo -> sendSuccess $ decodeUtf8 $ encode createdTodo
            Nothing -> sendError "should never happen. creating a todo returned an empty list" status500
        Nothing -> sendError "invalid input" status400

    -- UPDATE one todo
    patch "/todos/:id" $ do
      unparsedIdFromPath <- param "id"
      case decimal unparsedIdFromPath of
        Left err -> sendError (pack err) status400
        Right (idOfToBeUpdatedTodo, _rest) -> do
          unparsedJson <- body
          case decode unparsedJson :: Maybe UpdateTodoInput of
            Just updateTodoInput -> do
              if idOfToBeUpdatedTodo /= updateTodoInputId updateTodoInput
                then sendError "id of payload and path did not match" status400
                else do
                  updatedTodo <- liftIO (updateTodoById conn updateTodoInput)
                  case listToMaybe updatedTodo of
                    Just todo -> sendSuccess $ decodeUtf8 $ encode todo
                    Nothing -> sendError "not found" status404
            Nothing -> sendError "invalid input" status400

-- testing
spec :: IO Application -> Spec
spec application = H.with application $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      H.get "/" `H.shouldRespondWith` 200

    it "responds with 'hello'" $ do
      H.get "/" `H.shouldRespondWith` "hello"

    it "responds with 200 / 'hello'" $ do
      H.get "/" `H.shouldRespondWith` "hello" {H.matchStatus = 200}

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      H.get "/" `H.shouldRespondWith` 200 {H.matchHeaders = ["Content-Type" H.<:> "text/plain; charset=utf-8"]}

-- describe "GET /some-json" $ do
--   it "responds with some JSON" $ do
--     H.get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]
