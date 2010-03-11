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
					Re= re:compile(Regexstring) , %% might return {error, Error}
					{ReParseResult,_} = Re,		

					Url="http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="
						++ edoc_lib:escape_uri(Searchstring),
					
					Results = talk_to_google(Url, Re, [], 0),

					DataOut = mochijson2:encode( {struct, [
						{<<"results">>, Results}, 
						{<<"reparser">>, ReParseResult}]}),
					Req:ok({"application/json", [], [DataOut]} );
					
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.


talk_to_google(Url, Re, ResultsIn, Start ) ->
	UserAgent="bart's erlang google thingy",

	UrlWithStart=lists:flatten(Url ++ io_lib:format("&start=~p", [Start])),
	io:format("talk_to_google ~p ~p~n", [UrlWithStart, length(ResultsIn)]),

	{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =  
		http:request(get, {UrlWithStart, 
			[{"User-Agent", UserAgent}, {"Referer",
		"http://www.vandeenensupport.com"}]},[],[]),
	Struct = mochijson2:decode(Body),
	ResponseData=struct:get_value(<<"responseData">>, Struct),

	%% io:format("responseData = ~p ~n", [ResponseData]),
	case ResponseData of
		null -> 
			lists:reverse(ResultsIn);
		_ ->
			ResponseResults=struct:get_value(<<"results">>, ResponseData),
			% scan the results and filter with the regular expression
			Results= lists:flatten([interpret_data(ResponseResults, Re, [] )|ResultsIn]),

			if 
				length(Results) < 20 ->
					_CursorData = struct:get_value(<<"cursor">>, ResponseData),
					talk_to_google(Url, Re, Results, Start+4);
				true ->
					lists:reverse(Results)
			end		
		end.		

	

%% actual html search result data
interpret_data([R|Tail], Re, Acc ) ->
	case interpret_one_dataset(R, Re) of
		nomatch -> Acc1=Acc;
		V -> Acc1=[V|Acc]
	end,	
	interpret_data(Tail, Re, Acc1);

interpret_data([], _Re, Acc) ->
	Acc.


% one dataset is an array with various fields	
%% has valid compiled Re
interpret_one_dataset(R, Re) ->
	Content=struct:get_value(<<"content">>, R),
	S=binary_to_list(Content),
	case Re of 
		{ok, MP} ->
			case re:run(S,MP) of
				{match, _} -> R;
				nomatch -> nomatch
			end;	
		{error, _ErrorSpec} ->
			R
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
