#!/bin/sh
mkdir -p ../mochiweb/ebin
mkdir -p ebin

JQUERY_PURE_LIBS=../jQuery-pure-libs
test -f  priv/js/pure.js || cp $JQUERY_PURE_LIBS/js/* priv/js

erl -make
