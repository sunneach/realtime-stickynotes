%% @doc genrpc
%% a lightweight general server
%% purpose - hold a state of an individual connection
%% or connections themselves

-module(genrpc).
-export([start/2, init/3, rpc/2, loop/2]).
-include("debug_info.hrl").


start(Mod, State) ->
    Starter = self(),
    spawn(fun() ->
        Server = spawn_link(?MODULE, init, [self(), Mod, State]),
        receive
             _Any  -> Starter ! {ok, Server}
        after 2000 -> Starter ! {no_response, Server}
        end
    end),
    receive Any -> Any end.

init(From, Mod, State) ->
    From ! {init_start, self()},
    loop(Mod, State).

rpc(Server, Message) ->
    Server ! {self(), Message},
    receive 
      {answer, Answer} -> Answer;
      Unknown -> Unknown
    end.
    
loop(Mod, State) ->
    receive
        stop -> ok;
        {From,Msg} -> 
            {Answer, NewState} =
            try
                Mod:rpc_handler(Msg, State)
            catch _X:_Y -> 
                {bad_message, State}
            end,
            From ! {answer, Answer},
            loop(Mod, NewState);
        {'DOWN', MonitorReference, process, WS, Reason} ->
            ?DBG("down ~p: ~p",[WS,Reason]),
            {ok, NewState} = Mod:rpc_handler({remove, MonitorReference, WS, Reason}, State),
            loop(Mod, NewState)
    end.
