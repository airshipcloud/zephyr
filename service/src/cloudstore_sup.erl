-module(cloudstore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

rest() ->
    RestDispatch = [{'_', [
        {[], cloudstore, []}
    ]}],
    RestConfig = [rest_listener, 100,
        [{port, 10001}],
        [{dispatch, RestDispatch}]],
    {rest, {cowboy, start_http, RestConfig}, permanent, 5000, supervisor, dynamic}.

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
            {database, "cloudstore"},
            {username, "cloudstore"},
            {password, "y7DrtF48bc"}
        ]),
    {ok, { {one_for_one, 5, 10}, [Pool, rest()]} }.

