-- migrate:up
INSERT INTO todos (id, text, done) VALUES (1, 'do stuff', false), (2, 'review pr', true), (3, 'code in haskell', true);

-- migrate:down
DELETE FROM todos;

