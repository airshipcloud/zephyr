-module(router_services).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, undefined}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, get_json}], Req, State}.

get_json(Req, State) ->
    {ok, Routes} = file:read_file(filename:join(code:priv_dir(router), "routes.json")),
    {jiffy:encode(Routes), Req, State}.
