-module(googleajax).
-author('author <author@example.com>').

-compile(export_all).
-export([talk_to_google/2]).


%% send one http request to google, and when the result has been received,
% send the result to the owning process
start(Pid, Url, Re, Start) ->
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
			ResponseResults=struct:get_value(<<"results">>, ResponseData),
			Results= lists:reverse(interpret_data(ResponseResults, Re, [])),
			io:format("Page ~p matched ~p times~n", [Start, length(Results)]),
			R={struct, [ {<<"results">>, Results}]}
		end,
	% send information back to owning process
	Pid ! {self(), start, Start, R}.
	

% spawn google ajax processes an return a set of Pid's
spawn_http_requests(Url, Re) ->
	Pid=self(),
	L=lists:map( fun(Start) ->
		spawn(fun() -> start(Pid, Url, Re, Start) end) end, 
		lists:seq(0,20,4)),
	sets:from_list(L).

		
kill_http_request_processes(Pids) ->
	io:format("Killing ~p http processes~n", [sets:size(Pids)]),
	lists:map(fun(Pid) -> exit(Pid,kill) end, sets:to_list(Pids)).
	

collect_results(PageDict1, Acc) ->
	receive 
		{Pid, start, Start, Results} ->
			PageDict = sets:del_element(Pid, PageDict1),
			case sets:size(PageDict) of
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
		after 1500 	->
			io:format("timeout~n"),
			kill_http_request_processes(PageDict1),
			lists:keysort(2,Acc)
	end.

talk_to_google(Url, Re) ->
	PageDict = spawn_http_requests(Url, Re),
	Results=collect_results(PageDict, []), %% list of Re filtered page results
	%% create list of the results
	lists:flatten(lists:map(fun({start, _, R}) -> 
		struct:get_value(<<"results">>, R) end, Results)).


	
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

