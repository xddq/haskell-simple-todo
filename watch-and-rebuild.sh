#!/bin/bash

sh -c "cabal run todo-app &"

while inotifywait -qq -r -e modify ./app/* ./src/* ./test/* ; do killall todo-app; sh -c "cabal run todo-app &" ; done
