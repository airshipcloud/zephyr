-module(cloudstore).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([read_json/2]).
-export([write_json/2]).

-record(state, {path, hash}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

malformed_request(Req, State) ->
    {Path, Req0} = cowboy_req:path(Req),
    {false, Req0, State#state{path = Path}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

resource_exists(Req, #state{path = Path} = State) ->
    Q = <<"select hash from objects where hash=md5($1)">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Path]) of
        {ok, _, []} -> {false, Req, State};
        {ok, _, [{Hash}]} -> {true, Req, State#state{hash = Hash}}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, read_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, write_json}], Req, State}.

read_json(Req, #state{hash = Hash} = State) ->
    Q = <<"select value from objects where hash=$1">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Hash]) of
        {ok, _, []} -> {halt, Req, State};
        {ok, _, [{_Value}]} -> {jiffy:encode({[]}), Req, State}
    end.

write_json(Req, #state{hash = undefined, path = Path} = State) ->
    Q = <<"insert into objects(hash,version,path,value) values (md5($1),0,''::hstore,''::hstore)">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Path]),
    {true, Req, State};
write_json(Req, #state{hash = _Hash} = State) ->
    {true, Req, State}.
