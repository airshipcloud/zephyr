-module(cloudstore).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([read_json/2]).
-export([write_json/2]).
-export([delete_resource/2]).

-record(state, {path, segments, hash, hashes, etag, mode}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

parse_path(Path) ->
    Segments = [case Segment of "*" -> {expr, star}; _ -> list_to_binary(Segment) end || Segment <- string:tokens(binary_to_list(Path), "/")],
    case lists:any(fun ({expr, _}) -> true; (_) -> false end, Segments) of
        true -> {expr, Segments};
        false -> {pointer, Segments}
    end.

malformed_request(Req, State) ->
    {Path, Req0} = cowboy_req:path(Req),
    {Mode, Segments} = parse_path(Path),
    {false, Req0, State#state{path = Path, segments = Segments, mode = Mode}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

resource_exists(Req, #state{path = Path, mode = pointer} = State) ->
    Q = <<"select hash,md5(version::text) from objects where hash=md5($1)">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Path]) of
        {ok, _, []} -> {false, Req, State};
        {ok, _, [{Hash, ETag}]} -> {true, Req, State#state{hash = Hash, etag = ETag}}
    end;
resource_exists(Req, #state{segments = Segments, mode = expr} = State) ->
    Q = <<"select array_agg(hash),md5(array_to_string(array_agg(version::text), ' ')) from (select hash,version from objects where path @> hstore($1::text[]) order by path) o">>,
    {ok, _, [{Hashes, ETag}]} = cloudstore_pg:equery(cloudstore_pool, Q, [segments_to_hprops(Segments)]),
    {true, Req, State#state{hashes = Hashes, etag = ETag}}.

generate_etag(Req, #state{etag = undefined} = State) ->
    {undefined, Req, State};
generate_etag(Req, #state{etag = ETag} = State) ->
    {list_to_binary([<<"\"">>, ETag, <<"\"">>]), Req, State}.

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

hprops_to_path(HProps) ->
    R = hprops_to_path(HProps, []),
    list_to_binary(lists:append([["/", binary_to_list(Segment)] || {_, Segment} <- lists:keysort(1, R)])).

hprops_to_path([], R) -> R;
hprops_to_path(HProps0, R) ->
    {[Index, Segment], HProps} = lists:split(2, HProps0),
    hprops_to_path(HProps, [{list_to_integer(binary_to_list(Index)), Segment} | R]).

read_json(Req, #state{mode = pointer, hash = Hash} = State) ->
    Q = <<"select hstore_to_array(value) from objects where hash=$1">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Hash]) of
        {ok, _, []} -> {halt, Req, State};
        {ok, _, [{HProps}]} ->
            {jiffy:encode({hprops_to_props(HProps)}), Req, State}
    end;
read_json(Req, #state{mode = expr, hashes = Hashes} = State) ->
    Q = <<"select hstore_to_array(path), hstore_to_array(value) from objects where hash=any($1::text[]) order by path">>,
    {ok, _, R} = cloudstore_pg:equery(cloudstore_pool, Q, [Hashes]),
    {jiffy:encode({[{hprops_to_path(PathHProps), {hprops_to_props(ValueHProps)}} || {PathHProps, ValueHProps} <- R]}), Req, State}.

props_to_hprops(Props) ->
    lists:append([[Name, jiffy:encode(Value)] || {Name, Value} <- Props]).

segments_to_hprops(Segments) ->
    lists:append(lists:foldl(fun({expr, star}, R) -> [[] | R]; (Value, R) -> [[list_to_binary(integer_to_list(length(R))), Value] | R] end, [], Segments)).

write_json(Req, #state{mode = pointer, hash = undefined, path = Path, segments = Segments} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    {Props} = jiffy:decode(Json),
    Q = <<"insert into objects(hash,version,path,value) values (md5($1),0,hstore($2::text[]),hstore($3::text[]))">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Path, segments_to_hprops(Segments), props_to_hprops(Props)]),
    {true, Req0, State};

write_json(Req, #state{mode = pointer, hash = Hash} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    {Props} = jiffy:decode(Json),
    HProps = lists:append([[Name, jiffy:encode(Value)] || {Name, Value} <- Props]),
    Q = <<"update objects set version=version+1,value=value||hstore($2::text[]) where hash=$1">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Hash, HProps]),
    {true, Req0, State}.

delete_resource(Req, #state{mode = pointer, hash = Hash} = State) ->
    Q = <<"delete from objects where hash=$1">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [Hash]),
    {true, Req, State}.
