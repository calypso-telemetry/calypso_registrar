-module(cl_registrar_storage).
-author("Sergey Loguntsov").

%% API
-export([
  init/0,
  insert/2, lookup/1, delete/2, delete/1
]).

-define(BAG, registrar_bag).

init() ->
  ?BAG = ets:new(?BAG, [ named_table, bag, public, { write_concurrency, true }, { read_concurrency, true } ]).

insert(Key, Value) ->
  ets:insert(?BAG, {Key, Value}).

lookup(Key) ->
  [ Value || { _, Value } <- ets:lookup(?BAG, Key)].

delete(Key, Value) ->
  ets:delete(?BAG, { Key, Value }).

delete(Key) ->
  lists:foreach(fun(Item) ->
    ets:delete(?BAG, Item)
  end, ets:lookup(?BAG, Key)).
