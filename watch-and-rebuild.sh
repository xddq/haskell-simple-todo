#!/bin/bash

sh -c "cabal run todo-app &"

while inotifywait -qq -r -e modify ./app/* ; do killall todo-app; sh -c "cabal run todo-app &" ; done
