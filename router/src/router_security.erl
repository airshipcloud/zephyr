-module(router_security).

-export([add_cors/1]).
-export([add_cors_options/1]).

add_cors(Req) ->
    {ok, Origin} = application:get_env(router, origin),
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin,
    cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"OPTIONS, GET, PUT, DELETE">>,
    cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>,
    cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"origin, content-type, accept">>, Req)))).

add_cors_options(Req) ->
    cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"60">>, Req).
