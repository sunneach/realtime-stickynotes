%% DEBUG definition and conditional compilation instructions 
%% are included by all modules
-define(NYI(X),
  (begin
   io:format("*** NYI ~p ~p ~p~n",[?MODULE, ?LINE, X]),
   exit(nyi)
  end)).

-ifdef(debug_info).
    -define(DEBUG(X), io:format("DEBUG ~p ~p:~p ~p~n",[self(), ?MODULE, ?LINE, X])).
    -define(DBG(F,L), io:format("[DEBUG ~p ~p:~p] " ++ F ++ "~n",[self(), ?MODULE, ?LINE] ++ L)).
-else.
    -define(DEBUG(X), void).
    -define(DBG(F,L), void).
-endif.
 