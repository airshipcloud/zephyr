
-module(router_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([rest/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

rest() ->
    RestDispatch = cowboy_router:compile([{'_', [
        {"/services", router_services, []}
    ]}]),
    {ok, Port} = application:get_env(router, port),
    RestConfig = [rest_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, RestDispatch}]}]],
    {rest, {cowboy, start_http, RestConfig}, permanent, 5000, supervisor, [dynamic]}.

init([]) ->
    {ok, { {one_for_one, 5, 10}, [ rest() ] } }.
