# /bin/bash

rm ../serverside/wwwroot/*
elm make src/Main.elm --output ../serverside/wwwroot/main.js
browserify src/dates.js -o ../serverside/wwwroot/dates.js

cp src/app.js ../serverside/wwwroot
cp src/*.ico ../serverside/wwwroot
cp src/*.css ../serverside/wwwroot