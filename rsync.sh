#!/bin/sh

sed -i.bak '/livereload.js/d' index.html
rsync -rav -e ssh --checksum \
      --include 'index.html' \
      --include 'elm.js' \
      --include 'js/***' \
      --include 'css/***' \
      --include 'font/***' \
      --exclude '*' \
      . ub:~/elmreddit
mv index.html.bak index.html
