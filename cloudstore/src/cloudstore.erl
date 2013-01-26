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

-record(state, {path, segments, hash}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

malformed_request(Req, State) ->
    {Path, Req0} = cowboy_req:path(Req),
    Segments = [list_to_binary(L)  || L <- string:tokens(binary_to_list(Path), "/")],
    {false, Req0, State#state{path = Path, segments = Segments}}.

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

hprops_to_props(HProps) ->
    hprops_to_props(HProps, []).

hprops_to_props([], R) -> R;
hprops_to_props(HProps0, R) ->
    {[Name, Value], HProps} = lists:split(2, HProps0),
    hprops_to_props(HProps, [{Name, jiffy:decode(Value)} | R]).

read_json(Req, #state{hash = Hash} = State) ->
    Q = <<"select hstore_to_array(value) from objects where hash=$1">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Hash]) of
        {ok, _, []} -> {halt, Req, State};
        {ok, _, [{HProps}]} ->
            {jiffy:encode({hprops_to_props(HProps)}), Req, State}
    end.

props_to_hprops(Props) ->
    lists:append([[Name, jiffy:encode(Value)] || {Name, Value} <- Props]).

segments_to_hprops(Segments) ->
    lists:append(lists:foldl(fun(Value, R) -> [[list_to_binary(integer_to_list(length(R))), Value] | R] end, [], Segments)).

write_json(Req, #state{hash = undefined, path = Path, segments = Segments} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    {Props} = jiffy:decode(Json),
    Q = <<"insert into objects(hash,version,path,value) values (md5($1),0,hstore($2::text[]),hstore($3::text[]))">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Path, segments_to_hprops(Segments), props_to_hprops(Props)]),
    {true, Req0, State};
write_json(Req, #state{hash = Hash} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    {Props} = jiffy:decode(Json),
    HProps = lists:append([[Name, jiffy:encode(Value)] || {Name, Value} <- Props]),
    Q = <<"update objects set version=version+1,value=value||hstore($2::text[]) where hash=$1">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Hash, HProps]),
    {true, Req0, State}.
