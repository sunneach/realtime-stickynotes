@set MOCHIWEB_BIN_DIR=..\mochiweb\ebin
@if not exist %MOCHIWEB_BIN_DIR% mkdir %MOCHIWEB_BIN_DIR%
@if not exist ebin mkdir ebin

rem copy Javascript stash
@set JQUERY_PURE_LIBS=..\jQuery-pure-libs
@if not exist priv\js\pure.js  copy %JQUERY_PURE_LIBS%\js\*.* priv\js

erl -make
