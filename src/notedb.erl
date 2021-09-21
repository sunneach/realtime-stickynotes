%% @doc notedb  - a simple persistent DB engine
%% - holds sticky notes as an Erlang map.
%% - initial state:
%%   #{count=>0, notes=>#{}}
%% - stores the whole db into a human readable file
%% - upon startup, consults from the DB storage:
%%   

-module(notedb).
-export([start/0,create/1,insert/1,select/1,delete/1,update/1,next/0]).
-export([read_all/1]).
-export([loop/1,handle/4,extractId/1]).
-export([reset/0]).
-export([stop/0]).


-include("debug_info.hrl").

-define(DB_DIR_PREFIX,"Notedb").
-define(DB_STORAGE, "data.erl").
-define(DB_STORAGE_PRESENT,true).
-define(DB_STORAGE_MISSING,false).
-define(DB_DIR_PRESENT,true).
-define(DB_DIR_MISSING,false).
-define(ID_PRESENT,true).
-define(ID_MISSING,false).

start()->
    IsDBStoragePresent = filelib:is_file(db_storage_file()),
    Base = init_base(IsDBStoragePresent),
    (fun() ->
    Pid = spawn(?MODULE,loop,[Base]),
    register(notedbloop,Pid) end
    )().

init_base(?DB_STORAGE_PRESENT)->
    %% if storage file exists - load database from it. 
    %% Once per server run.
    {ok, [Base]} = file:consult(db_storage_file()),
    Base;
init_base(?DB_STORAGE_MISSING)->
    %% if storage file is missing - initiate database. 
    %% Once per server run.
    IsDBDirPresent  = filelib:is_dir(db_directory()),
    ensure_db_dir(IsDBDirPresent),
    Base = #{count=>0, notes=>#{}},
    save(Base).

ensure_db_dir(?DB_DIR_PRESENT) -> ok;
ensure_db_dir(?DB_DIR_MISSING) -> file:make_dir(db_directory()).

reset() -> 
   Base = #{count=>0, notes=>#{}},
   save(Base).

loop(Base) ->
    receive
        {From, stop, _, _} ->
             From ! stopped,
             exit(stopped);
        {From, Action, Id, Rec} ->
            {Res, NewBase} = handle(Action, Id, Rec, Base),
            From ! Res,
            save(NewBase),
            loop(NewBase);
        X ->
            ?DBG("Database problem:~p",[X]),
            exit(bad_request)
    end.

%% process_transaction_log(Base).
handle(commit,_,_,Base) -> {ok,Base};
handle(transaction,_,_,Base) -> {ok,Base};
handle(rollback,_,_,Base) -> {ok,Base};
handle(insert,Id,Rec,Base) ->
    #{notes:=Notes} = Base,
    IsIdFound = maps:is_key(Id,Notes),
    handle_insert(IsIdFound, Notes, Id, Rec, Base);
handle(update,Id,Rec,Base) -> 
    #{notes:=Notes} = Base,
    IsIdFound = is_tuple(maps:find(Id,Notes)),
    handle_update(IsIdFound, Notes, Id, Rec, Base);
handle(select,Id,_,Base) -> 
    #{notes:=Notes} = Base,
    RecordOrError = maps:find(Id,Notes),
    {RecordOrError, Base};
handle(delete,Id,_,Base) -> 
    #{notes:=Notes} = Base,
    NewBase = Base#{notes := maps:remove(Id,Notes)},
    {record_deleted, NewBase};
handle(read_all,_,_,Base) -> 
    #{notes:=Notes} = Base,
    ListWithKeys = maps:to_list(Notes),
    FlatList = lists:map(fun({_Id,X}) -> X end, ListWithKeys),
    {FlatList, Base};
handle(next,_,_,Base) -> 
    #{count:=Count} = Base,
    NewCount = Count + 1,
    NewBase = Base#{count := NewCount},
    {NewCount, NewBase}.

read_all(_) -> 
    rpc(read_all, void, void).

create(Rec) -> 
    Id = next(),
    structo:set(<<"id">>, Id, Rec),
    rpc(insert, Id, Rec).
    
insert(Rec) -> 
    Id = extractId(Rec),
    rpc(insert, Id, Rec).

handle_insert(?ID_PRESENT, _Notes, _Id, _Rec, Base) ->
    {key_taken, Base};
handle_insert(?ID_MISSING, Notes, Id, Rec, Base) ->
    %% a brand new Id - using =>
    RecNoAction = structo:del(<<"action">>,Rec),
    RecWithId = structo:set(<<"id">>,Id,RecNoAction),
    ActionWithId = structo:set(<<"id">>,Id,Rec),
    NewNotes = Notes#{Id => RecWithId},
    NewBase = Base#{notes:=NewNotes},
    {[ActionWithId], NewBase}.

handle_update(?ID_MISSING, _Notes, _Id, _Rec, Base) ->
    {record_missing, Base};
handle_update(?ID_PRESENT,  Notes,  Id,  Rec, Base)  -> 
    RecNoAction = structo:del(<<"action">>,Rec),
    NotesUpdated = Notes#{Id := RecNoAction},
    NewBase = Base#{notes := NotesUpdated},
    {ok, NewBase}.

select(Id) -> 
    rpc(select, Id, void).

delete(Msg) -> 
    Id = extractId(Msg),
    rpc(delete, Id, void).

update(Rec) -> 
    Id = extractId(Rec),
    rpc(update, Id, Rec).

next() -> rpc(next, void, void).

stop() -> rpc(stop, void, void).

rpc(Action, Id, Rec) ->
    notedbloop ! {self(), Action, Id, Rec},
    receive
         Result -> Result
    end.

save(Base) -> 
    file:write_file(db_storage_file(), io_lib:format("~p.~n", [Base])),
    Base.

extractId(Rec) -> 
    structo:get(<<"id">>, Rec).

db_directory() -> ?DB_DIR_PREFIX ++ "." ++ atom_to_list(node()).

db_storage_file() -> db_directory() ++ "/" ++ ?DB_STORAGE.