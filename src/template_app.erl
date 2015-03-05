%% @private
-module(template_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->

    StateEcho = sockjs_handler:init_state(
      <<"/echo">>, fun sock_handler:service_echo/3, state,
      [{response_limit, 4096}]),
    StateClose = sockjs_handler:init_state(
      <<"/close">>, fun sock_handler:service_close/3, state, []),
    StateAmplify = sockjs_handler:init_state(
      <<"/amplify">>, fun sock_handler:service_amplify/3, state, []),
    StateBroadcast = sockjs_handler:init_state(
      <<"/broadcast">>, fun sock_handler:service_broadcast/3, state, []),
    StateFilter = sockjs_handler:init_state(
      <<"/filter">>, fun sock_handler:service_filter/3, state, []),
    StateDWSEcho = sockjs_handler:init_state(
      <<"/disabled_websocket_echo">>, fun sock_handler:service_echo/3, state,
      [{websocket, false}]),
    StateCNEcho = sockjs_handler:init_state(
      <<"/cookie_needed_echo">>, fun sock_handler:service_echo/3, state,
      [{cookie_needed, true}]),
    VRoutes = [
      {"/", toppage_handler, []},
      {"/websocket", ws_handler, []},
      {"/static/[...]", cowboy_static,{dir, "priv/static"}},
      {<<"/echo/[...]">>, sockjs_cowboy_handler, StateEcho},
      {<<"/close/[...]">>, sockjs_cowboy_handler, StateClose},
      {<<"/amplify/[...]">>, sockjs_cowboy_handler, StateAmplify},
      {<<"/broadcast/[...]">>, sockjs_cowboy_handler, StateBroadcast},
      {<<"/filter/[...]">>, sockjs_cowboy_handler, StateFilter},
      {<<"/disabled_websocket_echo/[...]">>, sockjs_cowboy_handler,
        StateDWSEcho},
      {<<"/cookie_needed_echo/[...]">>, sockjs_cowboy_handler,
        StateCNEcho},
      {'_', ?MODULE, []}],
    Routes = [{'_', VRoutes}], % any vhost

    Dispatch = 
        cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(http, 100, [{port, 6667}],
                                    [{env, [{dispatch, Dispatch}]}]),
    template_sup:start_link().

stop(_State) ->
	ok.
