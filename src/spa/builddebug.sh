# /bin/bash

elm make src/Main.elm --output ../serverside/wwwroot/main.js
cp src/app.js ../serverside/wwwroot
cp src/style.css ../serverside/wwwroot