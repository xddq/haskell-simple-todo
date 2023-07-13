-- migrate:up
CREATE TABLE todos (
  id SERIAL PRIMARY KEY,
  text TEXT NOT NULL,
  done BOOLEAN NOT NULL
);

-- migrate:down
DROP TABLE IF EXISTS todos;
