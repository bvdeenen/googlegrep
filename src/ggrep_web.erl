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
					Re= re:compile(Regexstring) ,

					Url="http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="
						++ edoc_lib:escape_uri(Searchstring),
					UserAgent="bart's erlang google thingy",

					{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =  
						http:request(get, {Url, [{"User-Agent", UserAgent}, {"Referer",
						"http://www.vandeenensupport.com"}]},[],[]),
					Struct = mochijson2:decode(Body),
					ResponseData=struct:get_value(<<"responseData">>, Struct),
					ResponseResults=struct:get_value(<<"results">>, ResponseData),

					% scan the results and filter with the regular expression
					case Re of 
						{ok, MP} -> 
							Results= interpret_data(ResponseResults, MP, []);
						{error, ErrorSpec} ->
							io:format("ErrorSpec: ~p~n", [ErrorSpec]),
							Results = interpret_data(ResponseResults, [])
					end,		

					DataOut = mochijson2:encode( {struct, [{<<"results">>, Results}]}),
					Req:ok({"application/json", [], [DataOut]} );
					
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.



%% actual html search result data
interpret_data([R|Tail], Re, Acc ) ->
	V=interpret_one_dataset(R, Re),
	case V of
		nomatch -> Acc1=Acc;
		_ -> Acc1=[V|Acc]
	end,	
	interpret_data(Tail, Re, Acc1);

interpret_data([], _, Acc) ->
	lists:reverse(Acc).

interpret_data([R|Tail], Acc ) ->
	V=interpret_one_dataset(R),
	interpret_data(Tail, [V|Acc]);

interpret_data([], Acc) ->
	lists:reverse(Acc).

% one dataset is an array with various fields	
%% has valid compiled Re
interpret_one_dataset(R, MP) ->
	Content=struct:get_value(<<"content">>, R),
	io:format("One record=~n~p~n", [R]),
	S=binary_to_list(Content),
	case re:run(S,MP) of
		{match, _} -> R;
		nomatch -> nomatch
	end.	

interpret_one_dataset(R) ->
	Content=struct:get_value(<<"content">>, R),
	S=binary_to_list(Content),
	io:format("~p~n", [S]),
	S.

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
