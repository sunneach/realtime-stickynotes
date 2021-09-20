%% @doc client_keeper
%% - spawns a server as genrpc and keeps all user sessions there
%% - provides callback handlers to:
%% - registers each WebSocket connection and its properties into a map.
%% - allows to get the stored properties back
%% - unregisters dropped connections
%% - sends a reply to a given connection
%% - broadcasts (publishes) a message to all subscribers


-module(client_keeper).
-export([start/0,
         stop/0,
         rpc_handler/2,
         get/0,
         subscribe/1,
         publish/1]).

-include("debug_info.hrl").

start() ->
    spawn(fun() ->
        InitialState = #{},
        {_Status, KeeperPid} = genrpc:start(?MODULE, InitialState),
        register(keeper_srv,KeeperPid)
        end).

stop() -> keeper_srv ! stop.

subscribe(Props) ->
    ok = genrpc:rpc(keeper_srv,{subscribe, {self(), Props}}).

get() ->
    genrpc:rpc(keeper_srv,{get, self()}).

publish(Answer) -> 
    genrpc:rpc(keeper_srv,{publish, Answer, self()}).

rpc_handler({get, WS}, State) ->
    case maps:find(WS, State) of
        {ok, {_MonitorReference, Props}} -> {Props, State};
        error                            -> {{error, self()}, State}
    end;
rpc_handler({subscribe, {WS, Props}}, State) ->
    MonitorReference = erlang:monitor(process, WS),
    NewState = maps:put(WS, {MonitorReference, Props}, State),
    {ok, NewState};
rpc_handler({remove, MonitorReference, WS, _Reason}, State) ->
    demonitor(MonitorReference),
    NewState = maps:remove(WS, State),
    {ok, NewState};
rpc_handler({publish, Answer, Initiator}, State) ->
    {ok, maps:fold(fun(WS,{_,{Mod,_,ReplyChannel}} = V,Acc) ->
            try
                begin
                    cast_answer(WS =/= Initiator,Mod,Answer,ReplyChannel),
                    maps:put(WS, V, Acc)
                end
            catch
                 _:_ -> Acc
            end
        end, #{}, State)}.

cast_answer(true,Mod,Answer,ReplyChannel) -> 
    Mod:reply(Answer, ReplyChannel);
cast_answer(false,_,_,_) -> 
    ok.
