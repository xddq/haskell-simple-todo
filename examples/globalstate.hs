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

import qualified Blaze.ByteString.Builder as B
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default.Class
import qualified Data.Map.Strict as M
import Data.String
import Data.Text.Lazy (Text, pack, strip, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read
import GHC.Generics (Generic)
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

-- TODO: drop _ when decoding

instance FromJSON Todo

instance ToJSON Todo

type Id = Int

newtype AppState = AppState {todo :: M.Map Id Todo}

-- newtype AppState = AppState { tickCount :: Int }

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
  -- get "/" $ do
  --     c <- webM $ gets tickCount
  --     text $ fromString $ show c
  --
  get "/" $ do
    text $ fromString "Welcome to your todo list! You might want to query /todos instead :]"

  get "/todos" $ do
    c <- webM $ gets todo
    setHeader "Content-Type" "application/json"
    text $ strip $ fromString $ unpack $ decodeUtf8 $ encode $ M.foldr (:) [] c

  -- DELETE todo
  post "/todos/delete/:id" $ do
    unparsedId <- param "id"
    -- text id
    let myid = decimal unparsedId
    case myid of
      Left err -> text $ pack err
      Right (x, _) -> do
        webM $ modify $ \st -> st {todo = M.delete x $ todo st}
        redirect "/todos"

  -- CREATE todo
  post "/todos" $ do
    -- TODO (perhaps later..?): payload should/could be json with field "text" as string or text
    newTodo1 <- body
    -- TODO (perhaps later..?) validate/parse the incoming data.
    let newTodo = decodeUtf8 newTodo1
    todos <- webM $ gets todo
    let idOfNewTodo = 1 + fst (M.findMax todos)
    let createdTodo = Todo (unpack newTodo) idOfNewTodo
    webM $ modify $ \st -> st {todo = M.insert idOfNewTodo createdTodo $ todo st}
    setHeader "Content-Type" "application/json"
    text $ todoToJsonText createdTodo

-- TODO after adding todo as record: UPDATE todo
-- post "/todos/:id" $ do
--   unparsedId <- param "id"
--   -- text id
--   let myid = decimal unparsedId
--   case myid of
--     Left err -> text $ pack err
--     Right (x, _) -> do
--       webM $ modify $ \st -> st {todo = M.delete x $ todo st}
--       redirect "/todos"

-- NOTE: this was copied from ./bodyecho.hs
ioCopy :: IO BS.ByteString -> IO () -> (B.Builder -> IO ()) -> IO () -> IO ()
ioCopy reader close write flush = step >> flush
  where
    step = do
      chunk <- reader
      if (BS.length chunk > 0)
        then (write $ B.insertByteString chunk) >> step
        else close
