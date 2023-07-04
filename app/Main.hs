{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode, object, withObject, (.:), (.=))
import Data.Default.Class
import qualified Data.Map.Strict as M
import Data.String
import Data.Text.Lazy (Text, pack, strip, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404)
import Network.Wai.Middleware.RequestLogger
import Prelude.Compat
import Web.Scotty.Trans
import Prelude ()

data Todo = Todo
  { _text :: String,
    _id :: Int,
    _done :: Bool
  }
  deriving (Show, Generic)

instance FromJSON Todo

instance ToJSON Todo where
  toJSON (Todo {_text = _text, _id = _id, _done = _done}) =
    object
      [ "text" .= _text,
        "id" .= _id,
        "done" .= _done
      ]

-- Client facing errors. Whenever we get an error, we return additional
-- information in this format.
data Error = Error
  { _errorMessage :: Text
  }
  deriving (Show, Generic)

instance ToJSON Error where
  toJSON (Error {_errorMessage = _errorMessage}) =
    object
      ["message" .= _errorMessage]

-- for validating the todo we pass when creating a new todo
data CreateTodoInput = CreateTodoInput
  { __text :: String,
    __done :: Bool
  }
  deriving (Show, Generic)

instance FromJSON CreateTodoInput where
  parseJSON = withObject "CreateTodoInput" $ \obj -> do
    __text <- obj .: "text"
    __done <- obj .: "done"
    return (CreateTodoInput {__text = __text, __done = __done})

-- TODO: adapt so here we can have have either updateTodoText or updateTodoDone or both
data UpdateTodoInput = UpdateTodoInput
  {updateTodoText :: String, updateTodoDone :: Bool}
  deriving (Show, Generic)

instance FromJSON UpdateTodoInput where
  parseJSON = withObject "UpdateTodoInput" $ \obj -> do
    newText <- obj .: "text"
    newDone <- obj .: "done"
    return (UpdateTodoInput {updateTodoText = newText, updateTodoDone = newDone})

type Id = Int

newtype AppState = AppState {todo :: M.Map Id Todo}

instance Default AppState where
  def = AppState $ M.fromList [(0, Todo "code stuff" 0 False), (1, Todo "use map" 1 False)]

-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- effectively meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Also note: your monad must be an instance of 'MonadIO' for
-- Scotty to use it.
newtype WebM a = WebM {runWebM :: ReaderT (TVar AppState) IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
  sync <- newTVarIO def
  -- 'runActionToIO' is called once per action.
  let runActionToIO m = runReaderT (runWebM m) sync

  scottyT 3000 runActionToIO app

todoToJsonText :: Todo -> Text
todoToJsonText = fromString . unpack . decodeUtf8 . encode

sendError :: Text -> Status -> ActionT Text WebM ()
sendError message responseStatus = do
  -- TODO: perhaps set header globally?
  setHeader "Content-Type" "application/json"
  status responseStatus
  text $ decodeUtf8 $ encode $ Error message

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT Text WebM ()
app = do
  middleware logStdoutDev
  get "/" $ do
    text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

  -- GET all todos
  get "/todos" $ do
    c <- webM $ gets todo
    setHeader "Content-Type" "application/json"
    status status200
    text $ strip $ fromString $ unpack $ decodeUtf8 $ encode $ M.foldr (:) [] c

  -- GET todo
  get "/todos/:id" $ do
    unparsedId <- param "id"
    case decimal unparsedId of
      Left err -> sendError (pack err) status400
      Right (parsedId, _rest) -> do
        todos <- webM $ gets todo
        case M.lookup parsedId todos of
          Nothing -> sendError "no todo found for given id" status404
          Just existingTodo -> do
            setHeader "Content-Type" "application/json"
            status status200
            text $ strip $ fromString $ unpack $ decodeUtf8 $ encode existingTodo

  -- DELETE todo
  delete "/todos/:id" $ do
    unparsedId <- param "id"
    case decimal unparsedId of
      Left err -> sendError (pack err) status400
      Right (parsedId, _rest) -> do
        todos <- webM $ gets todo
        case M.lookup parsedId todos of
          Just toDeleteTodo -> do
            webM $ modify $ \st -> st {todo = M.delete parsedId $ todo st}
            status status200
            setHeader "Content-Type" "application/json"
            text $ strip $ fromString $ unpack $ decodeUtf8 $ encode toDeleteTodo
          Nothing -> sendError "no todo found for given id" status404

  -- CREATE todo
  post "/todos" $ do
    unparsedJson <- body
    case decode unparsedJson :: Maybe CreateTodoInput of
      Just createTodoInput -> do
        todos <- webM $ gets todo
        -- MAYBE: make them functions and use as "with"
        let idOfNewTodo = 1 + fst (M.findMax todos)
        let createdTodo = Todo (__text createTodoInput) idOfNewTodo False
        webM $ modify $ \st -> st {todo = M.insert idOfNewTodo createdTodo $ todo st}
        setHeader "Content-Type" "application/json"
        text $ todoToJsonText createdTodo
      Nothing -> do
        status status400
        text "invalid input"

  -- UPDATE todo
  patch "/todos/:id" $ do
    unparsedIdFromPath <- param "id"
    case decimal unparsedIdFromPath of
      Left err -> sendError (pack err) status400
      Right (idOfTodoToBeUpdated, _rest) -> do
        unparsedJson <- body
        case decode unparsedJson :: Maybe UpdateTodoInput of
          Just updateTodoInput -> do
            todos <- webM $ gets todo
            case M.lookup idOfTodoToBeUpdated todos of
              Just _existingTodo -> do
                let updatedTodo = Todo (updateTodoText updateTodoInput) idOfTodoToBeUpdated (updateTodoDone updateTodoInput)
                webM $ modify $ \st -> st {todo = M.insert idOfTodoToBeUpdated updatedTodo $ todo st}
                setHeader "Content-Type" "application/json"
                text $ todoToJsonText updatedTodo
              Nothing -> sendError "no todo found for given id" status404
          Nothing -> sendError "invalid input" status400
