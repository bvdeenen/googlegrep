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
				"ggrep" ->
					Data = Req:parse_post(),
					Searchstring = proplists:get_value("searchstring", Data),
					_Regexstring = proplists:get_value("regexstring", Data),

					URL="http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="
						++ edoc_lib:escape_uri(Searchstring),
					UA="bart's erlang google thingy",
					io:format("URL= ~p ~n", [URL]),

					{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =  
						http:request(get, {URL, [{"User-Agent", UA}, {"Referer",
						"http://www.vandeenensupport.com"}]},[],[]),
					Struct = mochijson2:decode(Body),
					{struct, L1} = Struct,
					%%io:format("~p ~n", [L1]),
					interpret(L1),
					Result = {struct, [{<<"google">>, <<"result">>}]},
					DataOut = mochijson2:encode(Result),
					Req:ok({"application/json", [], [DataOut]} );
					
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.


interpret([{<<"responseData">>, {struct, L}}|Tail]) ->
	interpret_results(L),
	interpret(Tail) ;

interpret([{<<"responseDetails">>, Details}|Tail]) ->
	io:format("interpret L= ~p ~n", [Details]),
	interpret(Tail) ;

interpret([{<<"responseStatus">>, Statuscode}|Tail]) ->
	io:format("responseStatus = ~p ~n", [Statuscode]),
	interpret(Tail) ;

interpret([]) ->
	io:format("ready~n").



interpret_results([{<<"results">>, ResultList}|Tail]) ->
	%%io:format("ResultList results: ~p~n", [ResultList]),
	interpret_data(ResultList),
	interpret_results(Tail);

interpret_results([{<<"cursor">>, ResultList}|Tail]) ->
	%%io:format("ResultList cursor: ~p~n", [ResultList]),
	{struct, L}=ResultList,
	%%io:format("ResultList L: ~p~n", [L]),
	interpret_cursor(L),
	interpret_results(Tail);

interpret_results([]) ->
	[].

%% actual html search result data
interpret_data([{struct, R}|Tail]) ->
	io:format("data: ~p ~n", [R]),
	interpret_data(Tail);
interpret_data([]) ->
	[].

%% cursor data
interpret_cursor([H|Tail]) ->
	io:format("cursor: ~p ~n", [H]),
	interpret_cursor(Tail);

interpret_cursor([]) ->
	[].


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
