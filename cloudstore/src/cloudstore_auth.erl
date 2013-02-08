-module(cloudstore_auth).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).

-export([malformed_request/2]).
-export([forbidden/2]).
-export([resource_exists/2]).

-export([process_post/2]).
-export([read_html/2]).
-export([write_form/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, undefined}.

malformed_request(Req, State) ->
    {false, Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"text/html">>, read_html}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>, write_form}], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

process_post(Req, State) ->
    {true, Req, State}.

read_html(Req, State) ->
    {ok, Html} = file:read_file(filename:join(code:priv_dir(cloudstore), "auth.html")),
    {Html, Req, State}.

write_form(Req, State) ->
    error_logger:info_msg("#0~n"),
    % {ok, Req0} = cowboy_req:reply(403, Req),
    {ok, Req, State}.
