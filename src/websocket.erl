%% @doc
%% The application entry point: A bridge to the `mochiweb' web server.
%%
-module(websocket).
-export([start/0, start_link/0, ws_request_handler/3, http_handler/1]).

-include("debug_info.hrl").

-define(WEBSOCKET_REQUEST,   true).
-define(REGULAR_REQUEST,     false).
-define(RECORD_DELETED,      true).
-define(RECORD_NOT_DELETED,  false).
-define(READ_ALL,            true).
-define(NOT_READ_ALL,        false).
-define(APPLICATION_REQUEST, true).
-define(FILE_REQUEST,        false).
-define(GET_ALL_REQUEST,     [<<"{\"action\":\"read_all\"}">>]).

%% @doc start is the main entry
%% - checks the DB status
%% - launches Mochiweb web server
%%
start() ->
    code:load_file(note_handler),
    spawn(
      fun () ->
              check_database(),
              start_link(),
              register(wsmw, self()),
              receive
                  stop -> 
                          notedb:stop(),
                          client_keeper:stop(),
                          io:format("notedb stopped.~nkeeper stopped.~n~p server stopped.~n",[?MODULE])
              end
      end).

check_database() ->
    notedb:start().
%    mnesia:start(),
%    IsDBCreated = is_db_created(),
%    ensure_created(IsDBCreated).
    
%is_db_created() ->
%    stickydb:check().

%ensure_created({atomic, ok}) -> 
%%    io:format("re-setting DB~n"),
%    stickydb:reset();
%ensure_created({aborted,_}) -> 
%    io:format("DB already exists~n").

%% @doc start_link launches
%% the Mochiweb web server, providing a callback
%% routine. 
%%
start_link() ->
    client_keeper:start(),
    io:format("~p: Listening at http://127.0.0.1:5001/~n",[self()]),
    mochiweb_http:start_link([
                              {name, client_access},
                              {loop, {?MODULE, http_handler, []}},
                              {port, 5001}
                             ]).

%% @doc http_handler is called first when the HTTP request arrives
%%       Request   -   the incoming request object
%%  Purpose:
%%       determine the connection needs to be upgraded to websocket
http_handler(Request) -> 
    RequestMethod = mochiweb_request:get(method, Request),
    process_request(RequestMethod, Request).

process_request('GET', Request) ->
    IsWebsocketRequest = is_a_websocket_request(Request),
    handle_request(IsWebsocketRequest, Request).

is_a_websocket_request(Request) ->
    Upgrade = mochiweb_request:get_header_value("Upgrade", Request),
    Upgrade =/= undefined
        andalso string:to_lower(Upgrade) =:= "websocket".

handle_request(?WEBSOCKET_REQUEST, Request) ->
    route(Request);
handle_request(?REGULAR_REQUEST, Request) ->
    serve_file(Request).    

%% @doc serve_file looks for a file 
%%  starting from the project root and serves it
serve_file(Request) ->
    URIPath = mochiweb_request:get(path,Request),
    LocalFileName = extract_local_path(URIPath),
    mochiweb_request:serve_file(LocalFileName,"priv",Request).

% assume index.html
extract_local_path("/") -> "index.html";
extract_local_path(URIPath) -> string:slice(URIPath,1).

%% @doc initialize websocket
%%
%% this branch upgrades the HTTP connection to websocket one:
%%     - it is called once per websocket life
%%     - route runs in the same process as the ws_req_handler
%%
%%  Incoming:
%%      Request  - the HTTP request
%%  Purpose:
%%      1. extract the desired backend application the Request path
%%      2. create a proxy process to talk to the backend
%%      3. store the proxy Pid against the current process id.
route(Request) ->
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
                                    Request, fun ?MODULE:ws_request_handler/3),
    
    Path = list_to_binary(mochiweb_request:get(path,Request)),
    <<"/websocket/",Modstr/binary>> = Path,
    Mod = binary_to_existing_atom(Modstr),
    {_Status, Server, All} = Mod:start([]),
    ok = client_keeper:subscribe({Mod, Server, ReplyChannel}),
    Mod:reply(All, ReplyChannel),
    ReentryWs(self()).

%% @doc ws_request_handler is called when 
%%    a new Websocket message arrives from the client
%%    - Request   -  the Websocket message from the client
ws_request_handler(Request,_,_) ->
    {Mod,Server,ReplyChannel} = client_keeper:get(),
    process_messages(Request,Mod,Server,ReplyChannel).

%% @doc process_messages
%%     - looks into the list of incoming actions
%%     - ignores all but the last one
%%     - processes the last one
%% sometimes a message is a list of actions,
%% so processing only the last one
process_messages([],_,_,_) -> ok;
process_messages([H|[]],Mod,Server,ReplyChannel) ->
    process_messages(H,Mod,Server,ReplyChannel);
process_messages([_|T],Mod,Server,ReplyChannel) ->
    process_messages(T,Mod,Server,ReplyChannel);
process_messages(Request,Mod,Server,ReplyChannel) ->
    Result = Mod:user_action(Request,Server),
    % ?DEBUG(Result),
    {Action,Answer} = Result,
    % ?DEBUG([Action,Answer]),
    make_reply(Answer =:= record_deleted,Mod,Server,ReplyChannel,
               Action,Answer).
        
make_reply(?RECORD_DELETED,Mod,Server,ReplyChannel,_Action,_Answer) ->
                Mod:reply(ok,ReplyChannel),
                broadcast(true, Mod, Server);
make_reply(?RECORD_NOT_DELETED,Mod,Server,ReplyChannel,Action,Answer) ->
                Mod:reply(Answer,ReplyChannel),
                broadcast(Action =/= read_all,Mod,Server).


%% @doc broadcast
%%     - reads all notes from the DB
%%     - sends them to all but the current websocket client
%%       but only if the requested action was not the read_all 
broadcast(?NOT_READ_ALL,_,_) -> ok;
broadcast(?READ_ALL,Mod,Server) ->
    {read_all,AllNotes} = Mod:user_action(?GET_ALL_REQUEST,Server),
    client_keeper:publish(AllNotes).
