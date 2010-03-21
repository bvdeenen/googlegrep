-module(googleajax).
-author('Bart van Deene').

-compile(export_all).
-export([talk_to_google/2]).
-define( HTMLTAG_RE, tagmatcher()).

tagmatcher() ->
	{ok, M} = re:compile("<[^>]+>",[multiline,dotall,ungreedy]),
	M.

%% send one http request to google, and when the result has been received
% and filtered, send the result to the owning process
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
			case Re of
				{error, _} -> Results=ResponseResults;
				{ok, MP} -> Results=
					lists:reverse(interpret_data(ResponseResults, MP, []))
			end,
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


collect_results(PageSet1, Acc) ->
	receive
		{Pid, start, Start, Results} ->
			PageSet = sets:del_element(Pid, PageSet1),
			H={start, Start, Results},
			case sets:size(PageSet) of
				0 ->
					io:format("all http processes have returned~n"),
					lists:keysort(2,[H|Acc]);
				_ ->
					case Results of
						null -> collect_results(PageSet, Acc);
						_ -> collect_results(PageSet, [H |Acc])
					end
				end;
		Unexpected ->
			io:format("Unexpected ~p~n", [Unexpected])
		after 1500 	->
			io:format("timeout~n"),
			kill_http_request_processes(PageSet1),
			lists:keysort(2,Acc)
	end.

talk_to_google(Url, Re) ->
	PageSet = spawn_http_requests(Url, Re),
	Results=collect_results(PageSet, []), %% list of Re filtered page results
	%% create list of the results
	L=lists:map(fun({start, _, R}) ->
		struct:get_value(<<"results">>, R) end, Results),
	lists:flatten(L).



%% actual html search result data
interpret_data([R|Tail], MP, Acc ) ->
	case interpret_one_dataset(R, MP) of
		nomatch -> Acc1=Acc;
		V       -> Acc1=[V|Acc]
	end,
	interpret_data(Tail, MP, Acc1);

interpret_data([], _MP, Acc) ->
	Acc.

% one dataset is an array with various fields
%% has valid compiled Re
interpret_one_dataset(R, MP) ->
	Content=struct:get_value(<<"content">>, R),
	% remove tags
	S=re:replace(Content, ?HTMLTAG_RE, "", [{return, list},global]),
	case re:run(S,MP) of
		{match, _} ->
			S2=re:replace(S, MP, "<span class='match'>&</span>",
				[{return,list}, global]),
			struct:set_value(<<"content">>, list_to_binary(S2), R);
		nomatch -> nomatch
	end.

