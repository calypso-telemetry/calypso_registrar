-module(calypso_registrar).
-author("begemot").

%% API
-export([
  register/1, register/2, register/3, unregister/1,
  processes/1, process/1, is_process/1,
  pid/1,
  apply/3, apply_map/3, apply_by_id/3
]).

-record(process, {
  module :: atom(),
  pid :: pid(),
  data :: term()
}).

-define(IS_PROCESS(Process), is_record(Process, process)).

-opaque process() :: #process{}.
-export_type([ process/0 ]).

%% API
-export([
  local_register/2, local_lookup/1, local_unregister/1
]).

-spec local_register(term(), term()) -> ok.
local_register(Key, Module) ->
  gproc:add_local_property(Key, Module),
  ok.

-spec local_lookup(term()) -> [{pid(), term()}].
local_lookup(Key) ->
  gproc:lookup_local_properties(Key).

local_unregister(Key) ->
  gproc:unreg({p,l,Key}).

register({Module, Id}) ->
  ?MODULE:register(Module, Id);
register({Module, Id, Data}) ->
  register(Module, Id, Data);
register(List) when is_list(List) ->
  lists:foreach(fun(Item) ->
    ?MODULE:register(Item)
  end, List).

register(Module, Id) ->
  register(Module, Id, []).

register(Module, Id, Data) when is_atom(Module), is_list(Data) ->
  case proplists:get_value(self(), processes(Id)) of
    undefined -> ok;
    [{_, Data}] -> error({ process_registered, Data})
  end,
  Process = #process{ pid = self(), module = Module, data = Data},
  true = gproc:add_local_property({process, Id}, Process),
  Process.

unregister(Id) ->
  %% TODO: Needs to delete key, not replace
  true = gproc:add_local_property({process, Id}, undefined),
  case processes(Id) of
    [] ->
      gproc:unreg({p, l, { process, Id }});
    _ -> ok
  end.

processes(Id) ->
  [ Item || { _, Val } = Item <- gproc:lookup_local_properties({process, Id}), Val =/= undefined ].

process(Id) ->
  case processes(Id) of
    [] -> undefined;
    [{ _, Process}] -> Process;
    Any -> error(need_one_process, [ Id, Any ])
  end.

is_process(#process{}) -> true;
is_process(_) -> false.

apply(#process{module = Module, data = Data } = Process, Fun, Args) when is_function(Fun) ->
  Info = { FunModule, FunName, Arity} = calypso_util:ensure_function_exported(Fun),
  case FunModule =/= Module of
    true -> error({bad_module, Process, Info });
    false -> ok
  end,
  case Arity+1 =/= length(Args)+length(Data) of
    true -> error({bad_arity, Process, Info });
    false -> ok
  end,
  ?MODULE:apply(Process, FunName, Args);

apply(#process{pid = Pid, module = Module, data = Data }, FunName, Args) when is_atom(FunName), is_list(Args) ->
  FunArgs = [ Pid | Data ++ Args ],
  erlang:apply(Module, FunName, FunArgs).

apply_map(Processes, FunName, Args) ->
  [{Process, ?MODULE:apply(Process, FunName, Args)} || Process <- Processes ].

apply_by_id(Id, FunName, Args) ->
  apply_map([ Proc || { _, Proc } <- processes(Id) ], FunName, Args).

pid(undefined) -> undefined;
pid(Process) when ?IS_PROCESS(Process) ->
  Process#process.pid.

