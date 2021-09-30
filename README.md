# realtime-stickynotes

[<img src="https://user-images.githubusercontent.com/96718/135454719-581051a2-1b79-49d9-b505-25d733d901da.JPG" width ="400" height="400">](https://vimeo.com/618870678)


If you connect several clients - the changes made by one of them will be immediately pushed to all others.

This implementation uses websockets with *mochiweb*: https://github.com/mochi/mochiweb

## Pre-requisites:

- Erlang OTP, (seen it working on OTP 24, with Google Chrome)

## steps to make it work
### Linux
```shell
mkdir notes
cd notes
git clone git clone git@github.com:sunneach/stickynotes.git
git clone git@github.com:suneach/jQuery-pure-libs.git
git clone git@github.com:mochi/mochiweb.git
cd stickynotes
~/makeit.sh
~/launch.sh
```
### Windows
```batch
mkdir notes
cd notes
git clone git clone git@github.com:sunneach/stickynotes.git
git clone git@github.com:suneach/jQuery-pure-libs.git
git clone git@github.com:mochi/mochiweb.git
cd stickynotes
makeit.bat
launch.bat
```
Done.

visit the http://localhost:5001.


