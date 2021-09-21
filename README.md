This is the implementation of websockets,
using mochiweb: https://github.com/mochi/mochiweb

Pre-requisites:

- Erlang OTP, (seen it working on OTP 24, with Google Chrome)
- mochiweb is downloaded into the same location 
  as the StickyNotes, e.g.:

  - path_to_repos/mochiweb
  - path_to_repos/stickynotes

Make sure the folders exist:

        path_to_repos/mochiweb/ebin
        path_to_repos/stickynotes/ebin
        
To compile, use the Emakefile. In the stickynotes folder:

        erl -make
        
To run it:

        erl -pa ebin -pa ../mochiweb/ebin -s websocket

On Windows, you can also use the 

    makeit.bat
    launch.bat


then navigate your browser to http://localhost:5001.


