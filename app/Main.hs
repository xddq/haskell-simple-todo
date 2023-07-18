-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- seems to be "desctructuring" from js/ts
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- to construct json easily with quasiquotes
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Reader
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import qualified Data.Aeson.QQ as JSONQQ
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import qualified Network.HTTP.Types as Network.HTTP.Types.Header
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
  { createTodoInputText :: Text,
    createTodoInputDone :: Bool
  }
  deriving (Show, Generic, Eq)

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
    updateTodoInputText :: Text,
    updateTodoInputDone :: Bool
  }
  deriving (Show, Generic, Eq)

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
  deriving (Show, Generic, ToRow, Eq)

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

-- Used for testing the application. Will add dummy data to the database.
seededTodos :: Value
seededTodos = [JSONQQ.aesonQQ| [{done: false, text: "do stuff"}, {done: true, text: "review pr"}, {done: true, text: "code in haskell"}] |]

-- Used for testing the application. Will remove all data from the todos table
-- in the database.
flushDb :: Connection -> IO Int64
flushDb conn = execute_ conn "DELETE FROM todos"

-- Used for testing the application. Deletes all data from the database and then
-- seeds it with dummy data. Will be run before each test to ensure we have the
-- same state in the db for each case.
prepareDbForTestCase :: Connection -> IO ()
prepareDbForTestCase conn = do
  _flushedRowCount <- flushDb conn
  case fromJSON seededTodos :: Result [CreateTodoInput] of
    Success parsedTodos -> mapM_ (createTodo conn) parsedTodos
    Error err -> error err

-- Used for testing the application.
isTodoEqualToCreateTodoInput :: Todo -> CreateTodoInput -> Bool
isTodoEqualToCreateTodoInput (Todo _ todoText todoDone) (CreateTodoInput createTodoText createTodoDone) = todoText == createTodoText && todoDone == createTodoDone

-- Used for testing the application.
compareSeededTodosWithDbTodos :: LBS.ByteString -> LBS.ByteString -> Maybe String
compareSeededTodosWithDbTodos fromSeedTodos fromDbTodos = do
  case decode fromSeedTodos :: Maybe [CreateTodoInput] of
    Nothing -> Just "could not decode seeded todos."
    Just decodedSeededTodos -> do
      case decode fromDbTodos :: Maybe [Todo] of
        Nothing -> Just $ show fromDbTodos -- TODO: why can't we decode the db todos here... ?
        Just decodedDbTodos -> do
          let areEqual = zipWith isTodoEqualToCreateTodoInput decodedDbTodos decodedSeededTodos
          if and areEqual then Nothing else Just "some todos were not equal"

main :: IO ()
main = do
  putStrLn $ LBS.unpack $ encode seededTodos
  conn <- connect defaultConnectInfo {connectHost = "localhost", connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}
  hspec $ before_ (prepareDbForTestCase conn) $ spec $ app conn

-- app
app :: Connection -> IO Application
app conn =
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
  -- test the "dummy" route
  describe "GET /" $ do
    it "responds with 200" $ do
      H.get "/" `H.shouldRespondWith` 200

    it "responds with 'Welcome to your todo list! You might want to query /todos instead :]'" $ do
      H.get "/" `H.shouldRespondWith` "Welcome to your todo list! You might want to query /todos instead :]"

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      H.get "/" `H.shouldRespondWith` 200 {H.matchHeaders = ["Content-Type" H.<:> "text/plain; charset=utf-8"]}

  describe "GET /todos" $ do
    it "returns all todos" $ do
      H.get "/todos" `H.shouldRespondWith` 200 {H.matchBody = H.MatchBody (todosEqualExpectedTodos $ encode seededTodos)}

todosEqualExpectedTodos :: LBS.ByteString -> [Network.HTTP.Types.Header.Header] -> LBS.ByteString -> Maybe String
todosEqualExpectedTodos expected _ = compareSeededTodosWithDbTodos expected

-- describe "GET /todos/id" $ do
--   it "responds with some JSON" $ do
--     H.get "/todos/1" `H.shouldRespondWith` 200 {H.matchBody = H.MatchBody (bodyEquals $ encode seededTodos)}

-- TODO: create a todo
-- describe "GET /todos/:id" $ do
--   it "responds with some JSON" $ do
--     H.get "/todos/1" `H.shouldRespondWith` 200 {H.matchBody = H.MatchBody (bodyEquals $ encode seededTodo)}

-- TODO: delete a todo
-- describe "GET /todos/:id" $ do
--   it "responds with some JSON" $ do
--     H.get "/todos/1" `H.shouldRespondWith` 200 {H.matchBody = H.MatchBody (bodyEquals $ encode seededTodo)}

-- TODO: update a todo
-- describe "PATCH /todos/:id" $ do
--   it "responds with some JSON" $ do
--     H.get "/todos/1" `H.shouldRespondWith` 200 {H.matchBody = H.MatchBody (bodyEquals $ encode seededTodo)}

-- Used to create a H.ResponseMatcher
-- Here "Nothing" is actually the correct case and "Just" is the error case.
bodyEquals :: LBS.ByteString -> [Network.HTTP.Types.Header.Header] -> LBS.ByteString -> Maybe String
bodyEquals expected _ actual
  | expected == actual = Nothing
  | otherwise = Just $ "Expected: " ++ show expected ++ "\n but got: " ++ show actual
