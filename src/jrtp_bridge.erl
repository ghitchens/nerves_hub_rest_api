%% jrtp_bridge - JSON/REST Transport Protocol Bridge
%% 
%% Supports the REST methodoy to access points on the hub, using JSON as
%% the notation of the state. 
%%
%% GET /a/point       Maps to hub:deltas        
%% PUT /a/point       Maps to hub:update

-module(jrtp_bridge).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([rfc7386_provider/2]).
-export([json_provider/2]).
-export([html_provider/2]).
-export([text_provider/2]).
-export([content_types_accepted/2]).
-export([rfc7386_acceptor/2]).
-export([json_acceptor/2]).
-export([json_to_erl/1, erl_to_json/1]).

init(_Transport, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, HandlerOpts) -> 
    {ok, Req, HandlerOpts}.

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
    {<<"application/merge-patch+json">>, rfc7386_provider},
    {<<"application/json">>, json_provider},
    {<<"text/html">>, html_provider},
    {<<"text/plain">>, text_provider}
   ], Req, State}.


rfc7386_provider(Req, State) ->
    Path= request_path(Req),
    {VersHeader, R2} = cowboy_req:header(<<"x-since-version">>, Req),
    Vreq = vheader_to_ver(VersHeader),
    {LongPoll, R3} = cowboy_req:header(<<"x-long-poll">>, R2),
    {LongPollTimeoutHeaderValue, R4} = cowboy_req:header(<<"x-long-poll-timeout">>, R3),
    LongPollTimeout = case LongPollTimeoutHeaderValue of 
                        undefined -> 30000;
                        N -> binary_to_integer(N)
                      end,
    {Vres, Tree} = case LongPoll of 
        undefined -> 
            hub:deltas(Vreq, Path);
        _AnyOtherValue -> 
            StartResult = case maps:find(on_wait_start, State) of
              {ok, WaitStartFn} -> WaitStartFn();
              _ -> nil
            end,
            hub:watch(Path,[]),
            R = wait_for_version_after(Vreq, Path, LongPollTimeout),
            hub:unwatch(Path),
            case maps:find(on_wait_end, State) of
              {ok, WaitEndFn} -> WaitEndFn(StartResult);
              _ -> nil
            end,
            R
    end,

    {VlockReq, _} = Vreq,
    {VlockRes, _} = Vres,

    R5 = cowboy_req:set_resp_header(<<"x-version">>, ver_to_vheader(Vres), R4),
    R6 = invoke_response_hook_if_present(R5, State),
    
    % decide based on whether lock changed whether or not to respond with
    % application/json or application/xml+json
    % REVIEW: really? is this working? 
    R7 = case VlockRes of
        VlockReq -> R6;
        _ -> cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, R6)
    end,    
    
    % manage correct response to conditional gets
    case {Vreq, Tree} of 
        {{undefined, 0}, []} -> %% unconditional request,, but empty
            {<< <<"">>/binary>>, R7, State};
        {_, []} -> %% conditional, but nothing modified, respond with 304
            {ok, R8} = cowboy_req:reply(304, [], R7),
            {halt, R8, State};
        _ -> %% non-empty response
            Body = erl_to_json(Tree),
            {<<Body/binary, <<"\n">>/binary>>, R7, State}
    end.

json_provider(Req, State) ->
    Path= request_path(Req),
    {Vres, Tree} = hub:deltas({undefined, 0}, Path),
    Req2 = cowboy_req:set_resp_header(<<"x-version">>, ver_to_vheader(Vres), Req),
    Req3 = invoke_response_hook_if_present(Req2, State),
    case Tree of 
        [] -> {<< <<"">>/binary>>, Req3, State};
        _ ->
            Body = erl_to_json(Tree),
            {<<Body/binary, <<"\n">>/binary>>, Req3, State}
    end.

html_provider(Req, State) ->
      Header= <<"<html><head><meta charset=\"utf-8\"></head><body><pre>">>,
      Footer= <<"</pre></body></html>">>,
      {Body, Reply, NewState} = json_provider(Req, State),
      {<<Header/binary, Body/binary, Footer/binary>>, Reply, NewState}.

text_provider(Req, State) -> 
      json_provider(Req, State).

% Given a version header string in the format "VLOCK:VER", return
% the version tuple that corresponds to that, for passing on to the hub
vheader_to_ver(VersionHeaderValue) -> 
    case VersionHeaderValue of 
        undefined -> {undefined, 0};
    	S -> 
            case 'Elixir.String':split(S, <<":">>) of 
		  [Vlock, VS] ->  {Vlock, binary_to_integer(VS)};
		  _ -> {undefined, 0}  % vlock missing or does not match
            end
    end.

% Given an integer version, build a version lock string for a header
% in the formatin "VLOCK:VER"
ver_to_vheader({Vlock, Ver}) ->
    BVer = list_to_binary(integer_to_list(Ver)),
    <<Vlock/binary, <<":">>/binary, BVer/binary>>.
     
%%%%%%%%%%%%%%%%%%%%%%%%%% acceptors (respond to PUT) %%%%%%%%%%%%%%%%%%%%%%%%%

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"merge-patch+json">>, []}, rfc7386_acceptor},
        {{<<"application">>, <<"json">>, []}, json_acceptor}
    ], Req, State}.

rfc7386_acceptor(Req, State) ->
    json_acceptor(Req, State).
    
json_acceptor(Req, State) ->
    {ok, RequestBody, Req1} = cowboy_req:body(Req),
    ProposedChanges = json_to_erl(RequestBody),
    case hub:request(request_path(Req1), ProposedChanges) of
        {changes, Vres, Changes} ->
            ChangeJson = erl_to_json(Changes),
            ResponseBody = <<ChangeJson/binary, <<"\n">>/binary>>,
            BVer = ver_to_vheader(Vres),
            Req2 = cowboy_req:set_resp_header(<<"x-version">>, BVer, Req1),
            Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
            {true, Req3, State};
        {nochanges, Vres, _Changes} ->
            BVer = ver_to_vheader(Vres),
            Req2 = cowboy_req:set_resp_header(<<"x-version">>, BVer, Req1),
            {ok, Req3} = cowboy_req:reply(304, [], Req2),
            {halt, Req3, State};
        ok ->  % async response
            {ok, Req3} = cowboy_req:reply(202, [], Req1),
            {halt, Req3, State};
        _ -> 
            {ok, Req3} = cowboy_req:reply(400, [], Req1),
            {halt, Req3, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% utility functionss %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_response_hook_if_present(Req, State) -> 
  case maps:find(json_provider_hook, State) of
      {ok, RespHookFn} -> RespHookFn(Req);
      _ -> Req
  end.

%% request_path(Request) -> List
%%
%% returns the path of the request as a list of binary tokens, like this:
%% /config/device_info -> [<<"config">>, <<"device_info">>]
%%
%% REVIEW:      Could likely be rewritten to avoid binary_to_list and list_to
%%                      binary conversions, improving performance.

request_path(Req) -> 
    {Tokens, _} = cowboy_req:path_info(Req),
    Tokens.
    % {RequestPath, _} = cowboy_req:path(Req),
    % Strings = string:tokens(binary_to_list(RequestPath), "/"),
    % lists:map(fun(X)->list_to_binary(X) end, Strings).

%% REVIEW PERF: consider not calling us on every change everywhere in hub
wait_for_version_after(Vreq, Path, LongPollTimeout) -> % {Vres, ChangeTree}
    case hub:deltas(Vreq, Path) of
        {Vreq, _} -> 
            receive
                _ -> wait_for_version_after(Vreq, Path, LongPollTimeout)
            after LongPollTimeout -> 
                hub:deltas(Vreq, Path)  % force final version on timeout
            end;
        {Vres, []} -> % there were changes, but not in state we care about
                      % REVIEW PERF SEEMS NEEDED WHICH SEEMS WRONG!
                      % COULD BE INDICITAVE OF MESSAGE STORM?
            wait_for_version_after(Vres, Path, LongPollTimeout);
        {Vres, ChangeTree} -> 
            {Vres, ChangeTree}
    end.    

erl_to_json(Term) ->
    jsx:encode(Term, [ {space, 1}, {indent, 2}, {pre_encode, fun deatomize/1}]).

json_to_erl(Json) ->
    case jsx:decode(Json, [relax, {labels, atom}, {post_decode, fun atomize/1}]) of
        {incomplete, _CompletionFn} -> throw(error);
        Erl -> Erl
    end.

% convert binary to atoms, if they're preceded by hash marks.

atomize(<< H:1/binary, B/binary>>) -> 
    case H of 
        <<"#">> -> binary_to_atom(B, utf8);
        _ -> <<H/binary, B/binary>>
    end;
atomize(X) -> X.

% convert an atom to binary, unless it's true, false or nil, or not an atom,

deatomize(true) -> true;
deatomize(false) -> false;
deatomize(null) -> null;
deatomize(A) when is_atom(A) -> atom_to_binary(A, utf8);
deatomize(A) -> A.

