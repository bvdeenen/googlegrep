%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for ggrep.

-module(ggrep_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
	application:start(inets),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
	application:stop(inets),
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
				% form action
				"ggrep" ->
					Data = Req:parse_post(),
					Searchstring = proplists:get_value("searchstring", Data),
					Regexstring = proplists:get_value("regexstring", Data),
					CaseSensitive = proplists:get_value("casesensitive", Data),
					case CaseSensitive of
						undefined -> Options = [caseless];
						_ -> Options = []
					end,	

					Re= re:compile(Regexstring, Options) , %% might return {error, Error}
					{ReParseResult,_} = Re,		

					Url="http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="
						++ edoc_lib:escape_uri(Searchstring),
					
					Results = googleajax:talk_to_google(Url, Re),
					DataOut = mochijson2:encode( struct:set_value(<<"reparser">>, ReParseResult, 
						{struct, [{<<"results">>,Results}]})),
					Req:ok({"application/json", [], [DataOut]} );
					
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.



%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.

%% vim:tw=0
