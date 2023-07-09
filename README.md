# Simple Todo App API in Haskell

Just a RESTful API representing a simple todo app backend written in Haskell
using scotty. The code was based on the scotty sample from
[here](https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs).

We just wanted to build something in Haskell and thought of something beginners
could just "pick up" and do. We had a few struggles but it was a fun experience
overall. We are uploading it since it could be helpful to some other beginners
out there.

The code stores the todos in memory and does therefore reset all todos whenever
restarted.

## Quickstart

- Ensure you have ghc and cabal installed. We used
  [ghcup](https://www.haskell.org/ghcup/) to get these and used ghc 9.2.7.
- Install dependencies and build the app `cabal build`
- Run the app `cabal run todo-app`
- Send requests against the API, an easy way is to just use the hosted openapi
  specification within the swagger editor
[here](https://xddq.github.io/haskell-simple-todo)
- For development it might be useful to run `bash watch-and-rebuild.sh` if you
  have inotify-watch installed it will then automatically rebuild whenever you
make and save changes to the app.

## API specifiction

An API specification was created using openapi. It is hosted via github pages
and can be found [here](https://xddq.github.io/haskell-simple-todo).

## Improvements

We would be happy about suggestions and improvements to the small code base. We
literally just tried to make the compiler (or rather the LSP) not shout at us
:D.

## Further ToDos

These are just the next steps one could do in order to improve the app. They are
not strictly planned since we both have other priorities.
- Persist todos to disk using a database. The package
  [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
looks decent, but we have no experience with any library so this is just a
guess.

Created by @vimpostor and @xddq in between the talks at ZuriHac 23 to at least
get our feet wet with some Haskell :p.
