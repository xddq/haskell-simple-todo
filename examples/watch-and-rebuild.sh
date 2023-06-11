#!/bin/bash
#
#

while inotifywait -qq -r -e modify . ; do killall scotty-globalstate; sh -c "cabal run scotty-globalstate &" ; done
