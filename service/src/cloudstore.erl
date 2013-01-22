-module(cloudstore).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([read_json/2]).

-record(state, {path}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

malformed_request(Req, State) ->
    {false, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, read_json}], Req, State}.

read_json(Req, #state{tag = Tag} = State) ->
    {jiffy:encode({[]}), Req, State}.
