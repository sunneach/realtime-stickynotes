@set MOCHIWEB_BIN_DIR=..\mochiweb\ebin
@if not exist %MOCHIWEB_BIN_DIR% mkdir %MOCHIWEB_BIN_DIR%
erl -make
