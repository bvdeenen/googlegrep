-module(googleajax).
-author('author <author@example.com>').

-compile(export_all).
%-export([start/1, stop/0, loop/2]).


%% send one http request to google, and when the result has been received,
% send the result to the owning process
start() ->
	receive 
		{Pid, Url, Re, Start} ->
			UserAgent="bart's erlang google thingy",

			UrlWithStart=lists:flatten(Url ++ io_lib:format("&start=~p", [Start])),

			io:format("Request ~p ~n", [UrlWithStart]),
			{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =  
				http:request(get, {UrlWithStart, [{"User-Agent", UserAgent}, 
					{"Referer", "http://www.vandeenensupport.com"}]},[],[]),
			Struct = mochijson2:decode(Body),
			ResponseData=struct:get_value(<<"responseData">>, Struct),
			case ResponseData of
				null -> 
					%% beyond the last google page
					io:format("Page ~p was empty~n", [Start]),
					R=null;
				_ ->
					io:format("Page ~p returned data~n", [Start]),
					ResponseResults=struct:get_value(<<"results">>, ResponseData),
					Results= lists:flatten(interpret_data(ResponseResults, Re, [] )),
					R={struct, [ {<<"results">>, Results}]}
				end,
			Pid ! {self(), start, Start, R}
	end.
	

spawn_http_requests(Url, Re) ->
	spawn_http_requests(Url, Re, 0, dict:new()).

spawn_http_requests(Url, Re, Start, PageDict1 ) ->
	Pid = spawn(fun googleajax:start/0),
	PageDict = dict:store(Pid, ok, PageDict1),
	%% io:format("created process ~p~n", [Pid]),
	Pid ! { self(), Url, Re, Start},

	if Start < 20 ->
		spawn_http_requests(Url, Re, Start+4, PageDict);
	true ->
		PageDict
	end.

		
kill_http_request_processes([Pid|PagePids]) ->
	io:format("killing ~p~n", [Pid]),
	exit(Pid, kill),
	kill_http_request_processes(PagePids);
	
kill_http_request_processes([]) ->
	ok.
	

collect_results(PageDict1, Acc) ->
	receive 
		{Pid, start, Start, Results} ->
			PageDict = dict:erase(Pid, PageDict1),
			%% size(PageDict) should also work!
			case length(dict:fetch_keys(PageDict)) of
				0 -> 
					io:format("all http processes have returned~n"),
					lists:keysort(2,Acc);
				_ ->	
					case Results of
						null -> collect_results(PageDict, Acc);
						_ -> collect_results(PageDict, [{start, Start, Results} |Acc])
					end	
				end;
		Unexpected ->
			io:format("Unexpected ~p~n", [Unexpected])
		after 15000 	->
			io:format("timeout~n"),
			kill_http_request_processes(dict:fetch_keys(PageDict1)),
			lists:keysort(2,Acc)
	end.

unpack_results([{start, _Start, Results} |Tail], Acc) when Results =/= null ->
	
	V=struct:get_value(<<"results">>, Results),
	unpack_results(Tail, [V|Acc]);

unpack_results([],Acc) ->
	%%io:format("Acc=~n~p~n", [Acc]),
	lists:reverse(lists:flatten(Acc)).
	
	
talk_to_google(Url, Re) ->
	PageDict = spawn_http_requests(Url, Re),
	Results=collect_results(PageDict, []), %% list of Re filtered page results
	Results1=unpack_results(Results, []),
	Results1.


	
%% actual html search result data
interpret_data([R|Tail], Re, Acc ) ->
	case interpret_one_dataset(R, Re) of
		nomatch -> Acc1=Acc;
		V       -> Acc1=[V|Acc]
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
		% invalid Re string
		{error, _ErrorSpec} ->
			R
	end.		

