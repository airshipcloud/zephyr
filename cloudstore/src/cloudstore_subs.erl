-module(cloudstore_subs).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([options/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([read_json/2]).
-export([write_json/2]).
-export([delete_resource/2]).

-record(state, {sub_id, sub}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, cloudstore_security:add_cors(Req), #state{}}.

path_to_hprops(Path) ->
    Segments = [case Segment of "*" -> {expr, star}; _ -> list_to_binary(Segment) end || Segment <- string:tokens(binary_to_list(Path), "/")],
    lists:append(lists:foldl(fun({expr, star}, R) -> [[] | R]; (Value, R) -> [[list_to_binary(integer_to_list(length(R))), Value] | R] end, [], Segments)).

hprops_to_path(HProps) ->
    R = hprops_to_path(HProps, []),
    list_to_binary(lists:append([["/", binary_to_list(Segment)] || {_, Segment} <- lists:keysort(1, R)])).

hprops_to_path([], R) -> R;
hprops_to_path(HProps0, R) ->
    {[Index, Segment], HProps} = lists:split(2, HProps0),
    hprops_to_path(HProps, [{list_to_integer(binary_to_list(Index)), Segment} | R]).

malformed_request(Req, State) ->
    {SubId, Req0} = cowboy_req:binding(sub_id, Req),
    {false, Req0, State#state{sub_id = SubId}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

options(Req, State) ->
    {ok, cloudstore_security:add_cors(Req), State}.

resource_exists(Req, #state{sub_id = SubId} = State) ->
    Q = <<"select hstore_to_array(path),callback from subs where id=$1">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [SubId]) of
        {ok, _, []} -> {false, Req, State};
        {ok, _, R} -> {true, Req, State#state{sub = [{hprops_to_path(Path), Callback} || {Path, Callback} <- R]}}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, read_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, write_json}], Req, State}.

read_json(Req, #state{sub = Sub} = State) ->
    {jiffy:encode({[{Path, Callback} || {Path, Callback} <- Sub]}), Req, State}.

write_json(Req, #state{sub_id = SubId} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    {Props} = jiffy:decode(Json),
    ok = lists:foreach(fun({Path, Callback}) ->
        Q = <<"insert into subs(id,path,callback) values ($1,hstore($2::text[]),$3)">>,
        {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [SubId, path_to_hprops(Path), Callback])
    end, Props),
    {true, Req0, State}.

delete_resource(Req, #state{sub_id = SubId} = State) ->
    Q = <<"delete from subs where id=$1">>,
    {ok, _} = cloudstore_pg:equery(cloudstore_pool, Q, [SubId]),
    {true, Req, State}.
