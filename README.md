# Simple Todo App API in Haskell

Just a RESTful API representing a simple todo app backend written in Haskell
using scotty and postgresql-simple. A simple docker-compose is used to spin up
the database. Written by beginners so it might be helpful to other beginners 🤞.

# Prerequisites

- Ensure you have ghc and cabal installed. We used
  [ghcup](https://www.haskell.org/ghcup/) to get these and used ghc 9.2.7 and
cabal version 3.6.2.0.
- dbmate installed for raw SQL migrations
  - [install docs](https://github.com/amacneil/dbmate#installation)
- docker and docker-compose installed
- libpq-dev installed (for ubuntu 20.04 run `apt install libpq-dev`) I think
  this was required to be able to build postgresql-simple..?

## Quickstart

- Set up environment variables `cp env.local .env`
- Start postgresql/database `docker-compose up -d`
- Create the database `dbmate create`
- Create the table for our todo-app and fill it with 5 sample todos `dbmate up`
- Install dependencies and build the app `cabal build`
- Run the app `cabal run todo-app`
- Send requests to the API, an easy way is to just use the hosted OpenAPI
  specification within the swagger editor
[here](https://xddq.github.io/haskell-simple-todo)
- For development it might be useful to run `bash watch-and-rebuild.sh` if you
  have inotify-watch installed it will then automatically rebuild whenever you
make and save changes to the app.

## OpenAPI specification

<img src="https://github.com/xddq/haskell-simple-todo/blob/main/openapi-sample.png" width="400">

The Openapi specification is hosted via github pages and is available
[here](https://xddq.github.io/haskell-simple-todo).

## Managing the database

- [dbmate](https://github.com/amacneil/dbmate) is used for migrations, check
  their docu there if in doubt.
- Run dbmate to create migrations. Use snake_case since this is the default for
  postgres and dbmate. E.g. `dbmate new add_origin_created_at_to_recipes`
- A file will created which looks like this

```
-- migrate:up


-- migrate:down

```

## Improvements

Happy about suggestions and improvements to the small code base.

## Further ToDos

These are just some possibly next steps one could do in order to improve the
app. They are not strictly planned since we both have other priorities.
- Create frontend for the app ..? Perhaps adapt to use an html templating
  library to implement it as server rendered web app? blaze-html seems like it
could be used, but that was just a quick search.
- Add users and implement basic/simple authZ and authN.
  - Perhaps implement via "log in with google" or similar using OAuth2/OIDC.
- Check out nix (or nix-shell?) to provide the dev setup. Did not read to much
  about it yet, might be a nonsense task.
- Format code with formatter (which one is commonly used in haskell..?) Ormoulu
  seems to be somewhat known/used, but it was just a quick search again.
  - Perhaps add format check as github action

Initially created by @vimpostor and @xddq in between the talks at ZuriHac 23 to
at least get our feet wet with some Haskell :p.
