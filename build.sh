#!/bin/sh
cp src/Config.elm src/Config.elm.bak && \
cp src/Config.elm.secret src/Config.elm && \
elm-make src/Main.elm --output elm.js
mv src/Config.elm.bak src/Config.elm
