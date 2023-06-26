#!/bin/bash

sh -c "cabal run todo-app &"

while inotifywait -qq -r -e modify . ; do killall scotty-globalstate; sh -c "cabal run todo-app &" ; done
