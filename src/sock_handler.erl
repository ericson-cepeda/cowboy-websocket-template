-module(sock_handler).

-export([service_echo/3]).
-export([service_close/3]).
-export([service_amplify/3]).
-export([service_broadcast/3]).
-export([service_filter/3]).

%% Codebase from https://github.com/sockjs/sockjs-erlang examples

service_echo(_Conn, init, state) -> {ok, state};
service_echo(Conn, {recv, Data}, state) -> Conn:send(Data);
service_echo(_Conn, closed, state) -> {ok, state}.

service_close(Conn, _, _State) ->
  Conn:close(3000, "Go away!").

service_amplify(Conn, {recv, Data}, _State) ->
  N0 = list_to_integer(binary_to_list(Data)),
  N = if N0 > 0 andalso N0 < 19 -> N0;
        true -> 1
      end,
  Conn:send(list_to_binary(
    string:copies("x", round(math:pow(2, N)))));
service_amplify(_Conn, _, _State) ->
  ok.

service_broadcast(Conn, init, _State) ->
  case ets:info(broadcast_table, memory) of
    undefined ->
      ets:new(broadcast_table, [public, named_table]);
    _Any ->
      ok
  end,
  true = ets:insert(broadcast_table, {Conn}),
  ok;
service_broadcast(Conn, closed, _State) ->
  true = ets:delete_object(broadcast_table, {Conn}),
  ok;
service_broadcast(_Conn, {recv, Data}, _State) ->
  ets:foldl(fun({Conn1}, _Acc) -> Conn1:send(Data) end,
    [], broadcast_table),
  ok.

%%
%% Filter messages with specific REGEX rules
%%
service_filter(Conn, init, _State) ->
  service_broadcast(Conn, init, _State),
  ok;
service_filter(Conn, closed, _State) ->
  service_broadcast(Conn, closed, _State),
  ok;
service_filter(_Conn, {recv, Data}, _State) ->
  %% Verify whether it is an special message and respond accordingly.
  case re:run(Data, "^\\\\(.*)") of
    {match, Captured} -> service_echo(_Conn, {recv, lists:flatten(io_lib:format("~p", [Captured]))}, _State);
    nomatch -> service_broadcast(_Conn, {recv, Data}, _State)
  end,
  ok.