-module(cloudstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Sup = cloudstore_sup:start_link(),
    {ok, Port} = application:get_env(cloudstore, port),
    error_logger:info_msg("Cloudstore listening on ~p", [Port]),
    Sup.

stop(_State) ->
    ok.
