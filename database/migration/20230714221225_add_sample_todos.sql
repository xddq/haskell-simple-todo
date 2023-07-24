-- migrate:up
INSERT INTO todos (id, text, done) VALUES (1, 'do stuff', false), (2, 'review pr', true), (3, 'code in haskell', true), (4, 'check out nix-shell', false), (5, 'try property based testing', false);

-- migrate:down
DELETE FROM todos;

