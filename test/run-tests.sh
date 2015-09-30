#!/bin/sh

set -ev

cd "$(dirname "$0")"

mkdir -p build/

elm-make --yes --output build/test1.js Test.elm
echo "Elm.worker(Elm.Main);" >> build/test1.js
echo "exit" | node build/test1.js

elm-make --yes --output build/test2.js FileTest.elm
echo "Elm.worker(Elm.Main);" >> build/test2.js
node build/test2.js

elm-make --yes --output build/test3.js BigString.elm
echo "Elm.worker(Elm.Main);" >> build/test3.js
node build/test3.js > /dev/null

elm-make --yes --output build/test4.js Stdin.elm
echo "Elm.worker(Elm.Main);" >> build/test4.js
INPUT=`cat <<EOF
Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
EOF`
RESULT=`echo "$INPUT" | node build/test4.js`
if [ ! "$INPUT" = "$RESULT" ]; then
    echo "Unexpected output: $RESULT"
    exit 1
fi

echo "Success"
