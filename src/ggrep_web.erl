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
					ResponseData=struct:get_value(<<"responseData">>, Struct),
					ResponseResults=struct:get_value(<<"results">>, ResponseData),
					Results={struct, [{<<"results">>, interpret_data(ResponseResults,[])}]},



					io:format("Results=~n~p~n", [Results]),


					DataOut = mochijson2:encode(Results),
					Req:ok({"application/json", [], [DataOut]} );
					
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.



%% actual html search result data
interpret_data([R|Tail], Acc) ->
	V=interpret_one_dataset(R),
	interpret_data(Tail, [V|Acc]);

interpret_data([], Acc) ->
	Acc.

% one dataset is an array with various fields	

interpret_one_dataset(R) ->
	struct:get_value(<<"content">>, R).


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
