%% jrtp_bridge - JSON/REST Transport Protocol Bridge
%% 
%% Supports the REST methodoy to access points on the hub, using JSON as
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



st_to_xsession(St) ->
  [ {hw_key, HwKey} ] = ets:lookup(config, hw_key),
  base64:encode(crypto:block_encrypt(blowfish_cfb64, 
	       St, <<00,00,00,00,00,00,00,00>>, HwKey)).

json_provider(Req, State) ->
    Path= request_path(Req),
    {VersHeader, _} = cowboy_req:header(<<"x-since-version">>, Req),
    Vreq = vheader_to_ver(VersHeader),
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

    {SetTime, _} = cowboy_req:header(<<"x-set-time">>, Req),
    Rx = cowboy_req:set_resp_header(<<"x-version">>, 
				 ver_to_vheader(Vres), Req),
    Req2 = case SetTime of
      undefined -> Rx;
      _Other ->
	cowboy_req:set_resp_header(<<"x-session">>, 
		st_to_xsession(SetTime), Rx)
    end,

    ConditionalGet = (Vreq > 0),
    case {ConditionalGet, Tree} of 
        {true, []} -> %% conditional, but nothing modified, respond with 304
            %% {<< <<"">>/binary>>, Req2, State};
            {ok, Req3} = cowboy_req:reply(304, [], Req2),
            {halt, Req3, State};
            %%{<< <<"\n">>/binary>>, cowboy_req:reply(304, [] Req2), State};
        {false, []} -> %% unconditional, but empty
            {<< <<"">>/binary>>, Req2, State};
        _ -> %% we have a real response
            Body = erl_to_json(Tree),
            {<<Body/binary, <<"\n">>/binary>>, Req2, State}
    end.

html_provider(Req, State) ->
    case request_path(Req) of
        [] ->  %% redirect to the /admin/index.html file for root requests
            {ok, Reply} = cowboy_req:reply(302, 
                [{<<"Location">>, <<"/panel/index.html">>}], Req),
            {ok, Reply, State};
        _Else ->
                        Header= <<"<html><head><meta charset=\"utf-8\"><title>NNI-212</title></head><body><pre>">>,
                        Footer= <<"</pre></body></html>">>,
            {Body, Reply, NewState} = json_provider(Req, State),
                        {<<Header/binary, Body/binary, Footer/binary>>, Reply, NewState}
    end.

text_provider(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.

% returns the global version lock string as a binary
hub_vlock() ->
    [ {vlock, Vlock} ] = ets:lookup(config, vlock),
    Vlock.

% Given a version header string in the format "VLOCK:VER", return
% the integer version number, if the version lock matches the current
% hub versionlock
vheader_to_ver(VersionHeaderValue) -> 
    String = 'Elixir.String',
    case VersionHeaderValue of 
        undefined -> 0;
    	S -> 
            Vlock = hub_vlock(),
            case String:split(S, <<":">>) of 
		  [Vlock, VS] ->  binary_to_integer(VS);
		  _ -> 0  % vlock missing or does not match
            end
    end.

% Given an integer version, build a version lock string for a header
% in the formatin "VLOCK:VER"
ver_to_vheader(Ver) ->
    BVer = list_to_binary(integer_to_list(Ver)),
    Vlock = hub_vlock(),
    <<Vlock/binary, <<":">>/binary, BVer/binary>>.
     
%%%%%%%%%%%%%%%%%%%%%%%%%% acceptors (respond to PUT) %%%%%%%%%%%%%%%%%%%%%%%%%

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, json_acceptor},
        {{<<"application">>, <<"x-firmware">>, []}, firmware_acceptor}
    ], Req, State}.

firmware_acceptor(Req, State) -> 
    {ok, RequestBody, _} = cowboy_req:body(Req),
    fw_programmer:program(RequestBody, autoupdate, "/dev/sda"),
    {true, Req, State}.

json_acceptor(Req, State) ->
    {ok, RequestBody, _} = cowboy_req:body(Req),
    ProposedChanges = json_to_erl(RequestBody),
        {changes, Vres, Changes} = hub:update(request_path(Req), ProposedChanges),
    ChangeJson = erl_to_json(Changes),
    ResponseBody = <<ChangeJson/binary, <<"\n">>/binary>>,
    BVer = ver_to_vheader(Vres),
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

%% PERF  consider not calling us on every change everywhere in hub
wait_for_version_after(Vreq, Path) -> % {Vres, ChangeTree}
    case hub:deltas(Vreq, Path) of
        {Vreq, _} -> 
            receive
                _ -> wait_for_version_after(Vreq, Path)
            after 30000 -> % 30 second timeout PERF
                hub:deltas(Vreq, Path)  % force final version on timeout
            end;
        {Vres, []} -> % there were changes, but not in state we care about
                      % REVIEW PERF SEEMS NEEDED WHICH SEEMS WRONG!
                      % COULD BE INDICITAVE OF MESSAGE STORM?
            wait_for_version_after(Vres, Path);
        {Vres, ChangeTree} -> 
            {Vres, ChangeTree}
    end.    

erl_to_json(Term) ->
    Fn = fun(X) when is_atom(X) -> 
        atom_to_binary(X, utf8); 
%%        << <<"#">>/binary, BinX/binary>>;
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

