-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- seems to be "desctructuring" from js/ts
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( createTodo,
    getTodos,
    getTodoById,
    updateTodoById,
    deleteTodo,
    Todo (Todo, todoDone, todoId, todoText),
    CreateTodoInput (CreateTodoInput, createTodoInputDone, createTodoInputText),
    UpdateTodoInput (UpdateTodoInput, updateTodoInputDone, updateTodoInputId, updateTodoInputText),
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)

-- for validating the todo we pass when creating a new todo
data CreateTodoInput = CreateTodoInput
  { createTodoInputText :: Text,
    createTodoInputDone :: Bool
  }
  deriving (Show, Generic)

instance ToRow CreateTodoInput where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow CreateTodoInput {createTodoInputDone, createTodoInputText} = toRow (createTodoInputDone, createTodoInputText)

-- for validating the todo we pass when updating a todo
data UpdateTodoInput = UpdateTodoInput
  { updateTodoInputId :: Int,
    updateTodoInputText :: Text,
    updateTodoInputDone :: Bool
  }
  deriving (Show, Generic)

instance ToRow UpdateTodoInput where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow UpdateTodoInput {updateTodoInputText, updateTodoInputDone, updateTodoInputId} = toRow (updateTodoInputText, updateTodoInputDone, updateTodoInputId)

data Todo = Todo
  { todoId :: Int,
    todoText :: Text,
    todoDone :: Bool
  }
  deriving (Show, Generic, ToRow)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

createTodo :: Connection -> CreateTodoInput -> IO [Todo]
createTodo conn = query conn "INSERT INTO todos (done,text) VALUES (?,?) RETURNING *"

getTodos :: Connection -> IO [Todo]
getTodos conn = query_ conn "SELECT id,text,done FROM todos"

getTodoById :: Connection -> Int -> IO [Todo]
getTodoById conn idOfTodo = query conn "SELECT id,text,done FROM todos WHERE id = ?" [idOfTodo]

updateTodoById :: Connection -> UpdateTodoInput -> IO [Todo]
updateTodoById conn = query conn "UPDATE todos SET text=?,done=? WHERE id=? RETURNING *"

deleteTodo :: Connection -> Int -> IO Int64
deleteTodo conn todoId = execute conn "DELETE FROM todos WHERE id = ?" (Only todoId)
