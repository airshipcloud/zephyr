-module(cloudstore_tokens).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([read_json/2]).

-record(state, {token, acl}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, cloudstore_security:add_cors(Req), #state{}}.

%%parse_path(Path) ->
%%    [case Segment of "*" -> {expr, star}; _ -> list_to_binary(Segment) end || Segment <- string:tokens(binary_to_list(Path), "/")].

hprops_to_path(HProps) ->
    R = hprops_to_path(HProps, []),
    list_to_binary(lists:append([["/", binary_to_list(Segment)] || {_, Segment} <- lists:keysort(1, R)])).

hprops_to_path([], R) -> R;
hprops_to_path(HProps0, R) ->
    {[Index, Segment], HProps} = lists:split(2, HProps0),
    hprops_to_path(HProps, [{list_to_integer(binary_to_list(Index)), Segment} | R]).

malformed_request(Req, State) ->
    {Token, Req0} = cowboy_req:binding(token, Req),
    {false, Req0, State#state{token = Token}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

resource_exists(Req, #state{token = Token} = State) ->
    Q = <<"select hstore_to_array(path),access from tokens where id=$1">>,
    case cloudstore_pg:equery(cloudstore_pool, Q, [Token]) of
        {ok, _, []} -> {false, Req, State};
        {ok, _, R} -> {true, Req, State#state{acl = [{hprops_to_path(Path), Access} || {Path, Access} <- R]}}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, read_json}], Req, State}.

read_json(Req, #state{acl = Acl} = State) ->
    {jiffy:encode({[{Path, Access} || {Path, Access} <- Acl]}), Req, State}.
