-module(cloudstore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

rest() ->
    RestDispatch = cowboy_router:compile([{'_', [
        {"/tokens/[:token]", cloudstore_tokens, []},
        {"/auth", cloudstore_auth, []},
        {'_', cloudstore, []}
    ]}]),
    {ok, Port} = application:get_env(cloudstore, port),
    RestConfig = [rest_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, RestDispatch}]}]],
    {rest, {cowboy, start_http, RestConfig}, permanent, 5000, supervisor, [dynamic]}.

init([]) ->
    Pool = poolboy:child_spec(cloudstore_pool,
        [
            {name, {local, cloudstore_pool}},
            {worker_module, cloudstore_pg},
            {size, 3},
            {max_overflow, 10}
        ],
        [
            {hostname, "127.0.0.1"},
            {database, "cf_cloudstore"},
            {username, "cf_cloudstore"},
            {password, "dhY8AGhJ3Z"}
        ]),
    {ok, { {one_for_one, 5, 10}, [Pool, rest()]} }.

