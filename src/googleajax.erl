-module(googleajax).
-author('Bart van Deene').

%-compile(export_all).
-export([talk_to_google/2]).

tagmatcher() ->
	{ok, MP} = re:compile("<[^>]+>",[multiline,dotall,ungreedy]),
	io:format("Compiled regular expression for html tags ~p~n", [MP]),
	MP.

%% send one http request to google, and when the result has been received
% and filtered, send the result to the owning process
spawn_one_http_to_google(Pid, Url, Re, Start, TagRe) ->
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
				{empty, _} -> Results=ResponseResults;
				{ok, MP} ->
					  Results=lists:filter( fun(R) -> R =/= nomatch end,
							lists:map( fun( R ) ->
								interpret_one_dataset(R, MP, TagRe) end,
								ResponseResults))
					
			end,
			io:format("Page ~p matched ~p times~n", [Start, length(Results)]),
			R={struct, [ {<<"results">>, Results}]}
		end,
	% send information back to owning process
	Pid ! {self(), start, Start, R}.

% spawn google ajax processes an return a set of Pid's
spawn_http_requests(Url, Re) ->
	Pid=self(),
	TagRe=get(tagre),
	L=lists:map( fun(Start) ->
		spawn(fun() ->
			spawn_one_http_to_google(Pid, Url, Re, Start, TagRe) end) end,
		lists:seq(0,20,4)),
	sets:from_list(L).

kill_http_request_processes(Pids) ->
	io:format("Killing ~p unfinished http processes~n", [sets:size(Pids)]),
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
						%% google returned null result, beyond last page
						null -> collect_results(PageSet, Acc);
						_    -> collect_results(PageSet, [H |Acc])
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
	case get(tagre) of
		undefined -> put(tagre, tagmatcher());
		_         -> ok
	end,

	PageSet = spawn_http_requests(Url, Re), %% PageSet is set of process id's
	Results=collect_results(PageSet, []), %% list of Re filtered page results
	lists:flatten(lists:map(fun({start, _, R}) ->
		struct:get_value(<<"results">>, R) end, Results)).


% one dataset is an array with various fields
%% has valid compiled Re
interpret_one_dataset(R, MP, TagRe) ->
	Content=struct:get_value(<<"content">>, R),
	% remove tags
	S=re:replace(Content, TagRe,  "", [{return, list},global]),
	case re:run(S,MP) of
		{match, _} ->
			S2=re:replace(S, MP, "<span class='match'>&</span>",
				[{return,list}, global]),
			struct:set_value(<<"content">>, list_to_binary(S2), R);
		nomatch -> nomatch
	end.

