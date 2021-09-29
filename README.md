# realtime-stickynotes

![InitialState](https://user-images.githubusercontent.com/96718/134396639-3bb8de57-2465-45a0-b4e4-01aba42f334b.JPG)

If you connect several clients - the changes made by one of them will be immediately pushed to all others.

This implementation uses the websockets with *mochiweb*: https://github.com/mochi/mochiweb

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


