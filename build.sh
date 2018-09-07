#!/bin/sh
cp src/Config.elm Config.elm.bak && \
cp Config.elm.secret src/Config.elm && \
elm make src/Main.elm --output elm.js
mv Config.elm.bak src/Config.elm
