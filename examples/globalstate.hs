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
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai.Middleware.RequestLogger
import Prelude.Compat
import Web.Scotty.Trans
import Prelude ()

-- TODO: make _text of type "Text" later
data Todo = Todo
  { _text :: String,
    _id :: Int
  }
  deriving (Show, Generic)

instance FromJSON Todo

instance ToJSON Todo where
  toJSON (Todo {_text = _text, _id = _id}) =
    object
      [ "text" .= _text,
        "id" .= _id
      ]

-- for validating the todo we pass when creating a new todo
data CreateTodoInput = CreateTodoInput
  { __text :: String
  }
  deriving (Show, Generic)

instance FromJSON CreateTodoInput where
  parseJSON = withObject "CreateTodoInput" $ \obj -> do
    __text <- obj .: "text"
    return (CreateTodoInput {__text = __text})

-- for validating the todo we pass when updating a todo
data UpdateTodoInput = UpdateTodoInput
  {updateTodoText :: String, updateTodoId :: Int}
  deriving (Show, Generic)

instance FromJSON UpdateTodoInput where
  parseJSON = withObject "UpdateTodoInput" $ \obj -> do
    newText <- obj .: "text"
    newId <- obj .: "id"
    return (UpdateTodoInput {updateTodoText = newText, updateTodoId = newId})

type Id = Int

newtype AppState = AppState {todo :: M.Map Id Todo}

instance Default AppState where
  def = AppState $ M.fromList [(0, Todo "code stuff" 0), (1, Todo "use map" 1)]

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

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT Text WebM ()
app = do
  middleware logStdoutDev
  get "/" $ do
    text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

  -- GET todos
  get "/todos" $ do
    c <- webM $ gets todo
    setHeader "Content-Type" "application/json"
    status status200
    text $ strip $ fromString $ unpack $ decodeUtf8 $ encode $ M.foldr (:) [] c

  -- GET todos
  get "/todos/:id" $ do
    unparsedId <- param "id"
    case decimal unparsedId of
      Left err -> do
        status status400
        text $ pack err
      Right (parsedId, _rest) -> do
        todos <- webM $ gets todo
        case M.lookup parsedId todos of
          Nothing -> do
            status status404
            text "not found"
          Just existingTodo -> do
            setHeader "Content-Type" "application/json"
            status status200
            text $ strip $ fromString $ unpack $ decodeUtf8 $ encode existingTodo

  -- DELETE todo
  post "/todos/delete/:id" $ do
    unparsedId <- param "id"
    case decimal unparsedId of
      Left err -> do
        status status400
        text $ pack err
      Right (parsedId, _rest) -> do
        todos <- webM $ gets todo
        case M.lookup parsedId todos of
          Just _existingTodo -> do
            webM $ modify $ \st -> st {todo = M.delete parsedId $ todo st}
            status status200
            text "success"
          Nothing -> do
            status status404
            text "not found"

  -- CREATE todo
  post "/todos" $ do
    unparsedJson <- body
    case decode unparsedJson :: Maybe CreateTodoInput of
      Just createTodoInput -> do
        todos <- webM $ gets todo
        -- MAYBE: make them functions and use as "with"
        let idOfNewTodo = 1 + fst (M.findMax todos)
        let createdTodo = Todo (__text createTodoInput) idOfNewTodo
        webM $ modify $ \st -> st {todo = M.insert idOfNewTodo createdTodo $ todo st}
        setHeader "Content-Type" "application/json"
        text $ todoToJsonText createdTodo
      Nothing -> do
        status status400
        text "invalid input"

  -- UPDATE todo
  post "/todos/:id" $ do
    unparsedIdFromPath <- param "id"
    case decimal unparsedIdFromPath of
      Left err -> do
        status status400
        text $ pack err
      Right (parsedIdFromPath, _rest) -> do
        unparsedJson <- body
        case decode unparsedJson :: Maybe UpdateTodoInput of
          Just updateTodoInput -> do
            let idOfUpdatedTodo = updateTodoId updateTodoInput
            if parsedIdFromPath /= idOfUpdatedTodo
              then do
                status status400
                text "id in path and in body are not equal"
              else do
                todos <- webM $ gets todo
                case M.lookup (updateTodoId updateTodoInput) todos of
                  Just _existingTodo -> do
                    let updatedTodo = Todo (updateTodoText updateTodoInput) idOfUpdatedTodo
                    webM $ modify $ \st -> st {todo = M.insert idOfUpdatedTodo updatedTodo $ todo st}
                    setHeader "Content-Type" "application/json"
                    text $ todoToJsonText updatedTodo
                  Nothing -> do
                    status status404
                    text "todo not found"
          Nothing -> do
            status status400
            text "invalid input"
