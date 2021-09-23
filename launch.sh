#/bin/sh
MOCHIWEB_BIN_DIR=../mochiweb/ebin
erl -pa ebin -pa $MOCHIWEB_BIN_DIR -s websocket
