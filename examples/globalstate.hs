{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
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

import Data.Default.Class
import Data.String
import Data.Text.Read
import Data.Text.Lazy (Text, strip)
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS

import Network.Wai.Middleware.RequestLogger

import Prelude ()
import Prelude.Compat

import Web.Scotty.Trans

type Todo = String
type Id = Int

newtype AppState = AppState { todo :: M.Map Id Todo}
-- newtype AppState = AppState { tickCount :: Int }

instance Default AppState where
    def = AppState $ M.fromList [(0, "code stuff"), (1,"use map")]
    -- def = AppState $ M.fromList []
    -- def = AppState 0

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
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
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
        text $ fromString $ "Welcome to your todo list! You might want to query /todos instead :]"

    get "/todos" $ do
        c <- webM $ gets todo
        text $ strip $ fromString $ M.foldr (\ curr acc -> concat[acc, curr, "\n"] ) "" c

    post "/todos/delete/:id" $ do
        id <- param "id"
        text id
        -- webM $ modify $ \ st -> st { todo = M.fromList [(0,"hi")] }
        -- TODO: find out how to get a value of type Int from a value of type
        -- Text in Haskell.
        webM $ modify $ \ st -> st { todo = M.delete id $ todo st }
        -- redirect "/todos"

    -- post "/echo" $ do
    --     rd <- bodyReader
    --     stream $ ioCopy rd $ return ()

    -- put "/todo" $ do
    --     v <- param "new-todo"
    --     webM $ modify $ \ st -> st { todo = v }
    --     redirect "/"
    --
    -- delete "/todo" $ do
    --     webM $ modify $ \ st -> st { todo = "" }
    --     redirect "/"

    -- get "/plusone" $ do
    --     webM $ modify $ \ st -> st { tickCount = 5 }
    --     redirect "/"
    --
    -- get "/plustwo" $ do
    --                              NOTE: ticketCount st + 2 gets the tickCount
    --                              from the state and then adds 2
    --     webM $ modify $ \ st -> st { tickCount = tickCount st + 2 }
    --     redirect "/"

-- NOTE: this was copied from ./bodyecho.hs
ioCopy :: IO BS.ByteString -> IO () -> (B.Builder -> IO ()) -> IO () -> IO ()
ioCopy reader close write flush = step >> flush where
   step = do chunk <- reader
             if (BS.length chunk > 0)
               then (write $ B.insertByteString chunk) >> step
               else close

