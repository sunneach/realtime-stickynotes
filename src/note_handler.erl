%% @doc note_handler 
%% - starts note server as genrpc
%% - makes inquries to the note server -> note db
%% - provides callback handlers to the note server

-module(note_handler).
-export([start/1, get_all/1, user_action/2, rpc_handler/2, reply/2]).

-include("debug_info.hrl").

start(State) ->
    {Status,Pid} = genrpc:start(?MODULE, State),
    spawn(fun() -> 
             receive _M
                  -> ?DBG("~p",[_M])
             end
          end),
    {Status,Pid,get_all(void)}.

get_all(_) ->
    notedb:read_all(void).

user_action(Request, Server) -> 
    call_rpc(Request, Server).
    
call_rpc({'EXIT',_}, Server) ->    
    genrpc:rpc(Server, invalid_request);
call_rpc(Request, Server) ->
    genrpc:rpc(Server, Request).

rpc_handler(EncodedMsg, State) ->
    {Action, Message} = decode_message(EncodedMsg),
    Answer = notedb:Action(Message),
    {{Action, Answer},State}.

decode_message(EncodedMsg) ->
    Message =  mochijson2:decode(EncodedMsg),
    ActionBinary = structo:get(<<"action">>, Message),
    Action = binary_to_existing_atom(ActionBinary),
    {Action, Message}.

reply(Msg, ReplyChannel) ->
    EncodedMsg = mochijson2:encode(Msg),
    ReplyChannel(EncodedMsg).

