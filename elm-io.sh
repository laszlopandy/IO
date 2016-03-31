#!/usr/bin/env bash
if [ "$#" = 3 ]; then
    MAIN_MODULE=$3
elif [ "$#" = 2 ]; then
    MAIN_MODULE=Main
else
    echo "Usage: $0 <generated-js-file> <output-file> [<Main>]"
    exit 1
fi

read -d '' handler <<- EOF
(function(){
    if (typeof Elm === "undefined") { throw "elm-io config error: Elm is not defined. Make sure you call elm-io with a real Elm output file"}
    if (typeof Elm.${MAIN_MODULE} === "undefined" ) { throw "Elm.${MAIN_MODULE} is not defined, make sure your module is named Main." };
    var worker = Elm.worker(Elm.${MAIN_MODULE});
})();
EOF

cat $1 > $2
echo "$handler" >> $2
