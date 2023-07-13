{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ReaderT to provide access
-- to a TVar containing global state.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module Main (main) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Default.Class
import Data.Int
import qualified Data.Map.Strict as M
import Data.String
import Data.Text.Lazy (Text, pack, strip, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Prelude.Compat
import Web.Scotty
import Prelude ()

-- Client facing errors. Whenever we get an error, we return additional
-- information in this format.
data ApiError = ApiError
  { apiErrrorMessage :: Text
  }
  deriving (Show, Generic)

instance ToJSON ApiError where
  toJSON (ApiError msg) =
    object
      ["message" .= msg]

--
-- for validating the todo we pass when creating a new todo
-- data CreateTodoInput = CreateTodoInput
--   { __text :: String,
--     __done :: Bool
--   }
--   deriving (Show, Generic)
--
-- instance FromJSON CreateTodoInput where
--   parseJSON = withObject "CreateTodoInput" $ \obj -> do
--     __text <- obj .: "text"
--     __done <- obj .: "done"
--     return (CreateTodoInput {__text = __text, __done = __done})
--
-- -- TODO: adapt so here we can have have either updateTodoText or updateTodoDone or both
-- data UpdateTodoInput = UpdateTodoInput
--   {updateTodoText :: Maybe String, updateTodoDone :: Maybe Bool}
--   deriving (Show, Generic)
--
-- instance FromJSON UpdateTodoInput where
--   parseJSON = withObject "UpdateTodoInput" $ \obj -> do
--     newText <- obj .:? "text"
--     newDone <- obj .:? "done"
--     return (UpdateTodoInput {updateTodoText = newText, updateTodoDone = newDone})

sendError :: Text -> Status -> ActionM ()
sendError message responseStatus = do
  -- TODO: perhaps set header globally?
  setHeader "Content-Type" "application/json"
  status responseStatus
  text $ decodeUtf8 $ encode $ ApiError message

data Todo = Todo
  { todoId :: Int,
    todoText :: Text,
    todoDone :: Bool
  }
  deriving (Show, Generic)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

getTodoById :: Connection -> Int -> IO [Todo]
getTodoById conn idOfTodo = query conn "SELECT * FROM todos WHERE id = ?" [idOfTodo]

-- TODO: why does this not work??
-- getTodoById conn idOfTodo = query conn "SELECT (id,text,done) FROM todos WHERE id = ?" [idOfTodo]

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {connectHost = "localhost", connectDatabase = "todo-app", connectUser = "psql", connectPassword = "psql"}

  scotty 3000 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev
    middleware allowCors

    get "/" $ do
      text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

    get "/4" $ do
      [Only result] <- liftIO (query_ conn "select 2 + 2" :: IO [Only Int])
      text $ pack $ show result

    get "/todos/:id" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> sendError (pack err) status400
        Right (parsedId, _rest) -> do
          -- TODO: why do we get error "Incompatible {errSQLType = "record",
          -- errSQLTableOid = Nothing, errSQLField = "row", errHaskellType =
          -- "Int", errMessage = "types incompatible"}" here?
          todo <- liftIO (getTodoById conn parsedId)
          setHeader "Content-Type" "application/json"
          status status200
          text $ decodeUtf8 $ encode todo

instance FromJSON Todo

instance ToJSON Todo where
  toJSON (Todo todoToJsonId todoToJsontext todoToJsonDone) =
    object
      [ "id" .= todoToJsonId,
        "text" .= todoToJsontext,
        "done" .= todoToJsonDone
      ]

-- -- GET all todos
-- get "/todos" $ do
--   c <- webM $ gets todo
--   setHeader "Content-Type" "application/json"
--   status status200
--   text $ strip $ fromString $ unpack $ decodeUtf8 $ encode $ M.foldr (:) [] c
--
-- -- GET todo
-- get "/todos/:id" $ do
--   unparsedId <- param "id"
--   case decimal unparsedId of
--     Left err -> sendError (pack err) status400
--     Right (parsedId, _rest) -> do
--       todos <- webM $ gets todo
--       case M.lookup parsedId todos of
--         Nothing -> sendError "no todo found for given id" status404
--         Just existingTodo -> do
--           setHeader "Content-Type" "application/json"
--           status status200
--           text $ strip $ fromString $ unpack $ decodeUtf8 $ encode existingTodo
--
-- -- DELETE todo
-- delete "/todos/:id" $ do
--   unparsedId <- param "id"
--   case decimal unparsedId of
--     Left err -> sendError (pack err) status400
--     Right (parsedId, _rest) -> do
--       todos <- webM $ gets todo
--       case M.lookup parsedId todos of
--         Just toDeleteTodo -> do
--           webM $ modify $ \st -> st {todo = M.delete parsedId $ todo st}
--           status status200
--           setHeader "Content-Type" "application/json"
--           text $ strip $ fromString $ unpack $ decodeUtf8 $ encode toDeleteTodo
--         Nothing -> sendError "no todo found for given id" status404
--
-- -- CREATE todo
-- post "/todos" $ do
--   unparsedJson <- body
--   case decode unparsedJson :: Maybe CreateTodoInput of
--     Just createTodoInput -> do
--       todos <- webM $ gets todo
--       -- MAYBE: make them functions and use as "with"
--       let idOfNewTodo = 1 + fst (M.findMax todos)
--       let createdTodo = Todo (__text createTodoInput) idOfNewTodo False
--       webM $ modify $ \st -> st {todo = M.insert idOfNewTodo createdTodo $ todo st}
--       setHeader "Content-Type" "application/json"
--       text $ todoToJsonText createdTodo
--     Nothing -> do
--       status status400
--       text "invalid input"
--
-- -- UPDATE todo
-- patch "/todos/:id" $ do
--   unparsedIdFromPath <- param "id"
--   case decimal unparsedIdFromPath of
--     Left err -> sendError (pack err) status400
--     Right (idOfTodoToBeUpdated, _rest) -> do
--       unparsedJson <- body
--       case decode unparsedJson :: Maybe UpdateTodoInput of
--         Just updateTodoInput -> do
--           todos <- webM $ gets todo
--           case M.lookup idOfTodoToBeUpdated todos of
--             Just currentTodo -> do
--               let updatedTodo = updateTodo currentTodo updateTodoInput
--               webM $ modify $ \st -> st {todo = M.insert idOfTodoToBeUpdated updatedTodo $ todo st}
--               setHeader "Content-Type" "application/json"
--               text $ todoToJsonText updatedTodo
--             Nothing -> sendError "no todo found for given id" status404
--         Nothing -> sendError "invalid input" status400

-- TODO: How is this done "correctly"? With this approach, we would need a lot
-- of different cases if we had e.g. 5 optional attributes..?
-- updateTodo :: Todo -> UpdateTodoInput -> Todo
-- updateTodo currentTodo newTodo = do
--   let newTodoText = updateTodoText newTodo
--   let newTodoDone = updateTodoDone newTodo
--   case (newTodoText, newTodoDone) of
--     (Just newText, Just newDone) -> Todo newText (_id currentTodo) newDone
--     (Just newText, Nothing) -> Todo newText (_id currentTodo) (_done currentTodo)
--     (Nothing, Just newDone) -> Todo (_text currentTodo) (_id currentTodo) newDone
--     (Nothing, Nothing) -> currentTodo

-- todoToJsonText :: Todo -> Text
-- todoToJsonText = fromString . unpack . decodeUtf8 . encode

-- sendError :: Text -> Status -> ActionT Text WebM ()
-- sendError message responseStatus = do
--   -- TODO: perhaps set header globally?
--   setHeader "Content-Type" "application/json"
--   status responseStatus
--   text $ decodeUtf8 $ encode $ ApiError message

-- TODO: why is this not working?
-- allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PATCH", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }
