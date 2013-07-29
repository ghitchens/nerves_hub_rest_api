%% jrtp_bridge - JSON/REST Transport Protocol Bridge
%% 
%% Supports the REST methodology to access points on the hub, using JSON as
%% the notation of the state. 
%%
%% GET /a/point       Maps to hub:deltas        
%% PUT /a/point       Maps to hub:update
%%
%% LICENSE
%%
%% Repurposed from RemoteRadio, Copyright © 1996-2012 Garth Hitchens, KG7GA,
%% and Echo, Copyright © 2012-2013 Garth Hitchens, All Rights Reserved
%% 
%% License explicitly granted to Rose Point Navigation Systems, LLC, for use 
%% in the NEMO network translator box.   For other uses contact the Author.
%%
%% vim:et,ts=4,sts=4

-module(jrtp_bridge).

-export([init/3]).

-export([allowed_methods/2]).
-export([resource_exists/2]).

-export([content_types_provided/2]).
-export([json_provider/2]).
-export([html_provider/2]).
-export([text_provider/2]).

-export([content_types_accepted/2]).
-export([json_acceptor/2]).

-export([json_to_erl/1, erl_to_json/1]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%% cowboy REST helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% does the point specified in the URL currently exist on the hub?
resource_exists(Req, State) ->
        case hub:fetch(request_path(Req)) of 
          {_, error} -> {false, Req, State };
          {_, _} -> {true, Req, State}
        end.

allowed_methods(Req, State) ->
        {[<<"GET">>, <<"PUT">>], Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% providers (respond to GET) %%%%%%%%%%%%%%%%%%%%%%%%%

content_types_provided(Req, State) -> 
        {[  
                {<<"application/json">>, json_provider},
                {<<"text/html">>, html_provider},
                {<<"text/plain">>, text_provider}
        ], Req, State}.

json_provider(Req, State) ->
    Path= request_path(Req),
    {SinceVersion, _} = cowboy_req:header(<<"x-since-version">>, Req),
    Vreq = case SinceVersion of 
        undefined -> 0;
        S -> list_to_integer(binary_to_list(S))
    end,
    {LongPoll, _} = cowboy_req:header(<<"x-long-poll">>, Req),
    {Vres, Tree} = case LongPoll of 
        undefined -> 
            hub:deltas(Vreq, Path);
        _AnyOtherValue -> 
            hub:watch(Path,[]),
            R = wait_for_version_after(Vreq, Path),
            hub:unwatch(Path),
            R
    end,

    %% encode the response version and tree
    BVer = list_to_binary(integer_to_list(Vres)),
    Req2 = cowboy_req:set_resp_header(<<"x-version">>, BVer, Req),
    Body = erl_to_json(Tree),
    {<<Body/binary, <<"\n">>/binary>>, Req2, State}.

html_provider(Req, State) ->
    case request_path(Req) of
        [] ->  %% redirect to the /admin/index.html file for root requests
            {ok, Reply} = cowboy_req:reply(302, 
                [{<<"Location">>, <<"/admin/index.html">>}], Req),
            {ok, Reply, State};
        _Else ->
                        Header= <<"<html><head><meta charset=\"utf-8\"><title>NNI-212</title></head><body><pre>">>,
                        Footer= <<"</pre></body></html>">>,
            {Body, Reply, NewState} = json_provider(Req, State),
                        {<<Header/binary, Body/binary, Footer/binary>>, Reply, NewState}
    end.

text_provider(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% acceptors (respond to PUT) %%%%%%%%%%%%%%%%%%%%%%%%%

content_types_accepted(Req, State) ->
        {[
                {{<<"application">>, <<"json">>, []}, json_acceptor}
        ], Req, State}.

json_acceptor(Req, State) ->
    {ok, RequestBody, _} = cowboy_req:body(Req),
    ProposedChanges = json_to_erl(RequestBody),
        {changes, Vres, Changes} = hub:update(request_path(Req), ProposedChanges),
    ChangeJson = erl_to_json(Changes),
    ResponseBody = <<ChangeJson/binary, <<"\n">>/binary>>,
    BVer = list_to_binary(integer_to_list(Vres)),
    Req2 = cowboy_req:set_resp_header(<<"x-version">>, BVer, Req),
    Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
    {true, Req3, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% utility functionss %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% request_path(Request) -> List
%%
%% returns the path of the request as a list of binary tokens, like this:
%% /config/device_info -> [<<"config">>, <<"device_info">>]
%%
%% REVIEW:      Could likely be rewritten to avoid binary_to_list and list_to
%%                      binary conversions, improving performance.

request_path(Req) ->
    {RequestPath, _} = cowboy_req:path(Req),
    Strings = string:tokens(binary_to_list(RequestPath), "/"),
    lists:map(fun(X)->list_to_binary(X) end, Strings).

wait_for_version_after(Vreq, Path) -> % {Vres, ChangeTree}
    case hub:deltas(Vreq, Path) of
        {Vreq, _} -> 
            receive
                _ -> wait_for_version_after(Vreq, Path)
            after 30000 -> % 30 second timeout
                { Vreq, [] }
            end;
        {Vres, ChangeTree} -> {Vres, ChangeTree}
    end.    

erl_to_json(Term) ->
    Fn = fun(X) when is_atom(X) -> 
        BinX = atom_to_binary(X, utf8), 
        << <<"#">>/binary, BinX/binary>>;
    (X) -> 
        X
    end,
    jsx:encode(Term, [ {space, 1}, {indent, 2}, {pre_encode, Fn}]) .

json_to_erl(Json) ->
    Fn = fun(X) when is_binary(X) -> 
        atomize(X);
    (X) -> 
        X
    end,
    case jsx:decode(Json, [relax, {labels, atom}, {post_decode, Fn}]) of
        {incomplete, _CompletionFn} -> throw(error);
        Erl -> Erl
    end.

atomize(<< H:1/binary, B/binary>>) -> 
    case H of 
        <<"#">> -> binary_to_atom(B, utf8);
        _ -> <<H/binary, B/binary>>
    end;
atomize(X) -> X.

