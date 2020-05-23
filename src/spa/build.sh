#! /bin/bash

set -e
DEST=${1:-dist}

echo "Romoving old app."
[ -d $DEST ] && rm -r $DEST/*

SECONDS=0
echo "building to $DEST"
echo "Transpiling to js."
elm make src/Main.elm --optimize --output="${DEST}/main.js"
echo "Output $(wc -c ${DEST}/main.js) bytes"

SECONDS=0
echo "Minifying elm..."
npx uglifyjs "${DEST}/main.js" --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output="${DEST}/main.js"
echo "Output $(wc -c ${DEST}/main.js) bytes."

echo "Building dates."
npx browserify src/dates.js -o $DEST/dates.js
echo "Output $(wc -c ${DEST}/dates.js) bytes"

echo "Minifying dates"
npx uglifyjs "${DEST}/dates.js" --output "${DEST}/dates.js" 
echo "Output $(wc -c ${DEST}/dates.js) bytes"

cp src/*.css ${DEST}
cp src/*.ico $DEST
cp src/app.js $DEST

echo "Finished build of spa."
echo $(ls $DEST)