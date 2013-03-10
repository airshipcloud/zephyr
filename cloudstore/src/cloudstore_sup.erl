-module(cloudstore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

rest() ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/auth", cloudstore_auth, []},
        {"/subscriptions/[:sub_id]", cloudstore_subs, []},
        {'_', cloudstore, []}
    ]}]),
    {ok, Port} = application:get_env(cloudstore, port),
    Config = [rest_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]],
    {rest, {cowboy, start_http, Config}, permanent, 5000, supervisor, [dynamic]}.

tokens() ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/tokens/[:token]", cloudstore_tokens, []}
    ]}]),
    {ok, Port} = application:get_env(cloudstore, tokenstore_port),
    {ok, IP} = application:get_env(cloudstore, tokenstore_ip),
    Config = [tokenstore_listener, 100,
        [
            {ip, IP},
            {port, Port}
        ],
        [{env, [{dispatch, Dispatch}]}]],
    {tokens, {cowboy, start_http, Config}, permanent, 5000, supervisor, [dynamic]}.

init([]) ->
    {ok, Hostname} = application:get_env(cloudstore, db_hostname),
    {ok, Database} = application:get_env(cloudstore, db_database),
    {ok, Username} = application:get_env(cloudstore, db_username),
    {ok, Password} = application:get_env(cloudstore, db_password),
    Pool = poolboy:child_spec(cloudstore_pool,
        [
            {name, {local, cloudstore_pool}},
            {worker_module, cloudstore_pg},
            {size, 3},
            {max_overflow, 10}
        ],
        [
            {hostname, Hostname},
            {database, Database},
            {username, Username},
            {password, Password}
        ]),
        Rest = rest(),
        Tokens = tokens(),
    {ok, { {one_for_one, 5, 10}, [Pool, Rest, Tokens]} }.

