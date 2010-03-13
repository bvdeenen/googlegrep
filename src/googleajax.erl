-module(googleajax).
-author('author <author@example.com>').

-compile(export_all).
%-export([start/1, stop/0, loop/2]).


start(Url, _Re, Start) ->
	UserAgent="bart's erlang google thingy",

	UrlWithStart=lists:flatten(Url ++ io_lib:format("&start=~p", [Start])),

	{ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =  
		http:request(get, {UrlWithStart, [{"User-Agent", UserAgent}, 
			{"Referer", "http://www.vandeenensupport.com"}]},[],[]),
	Struct = mochijson2:decode(Body),
	struct:get_value(<<"responseData">>, Struct).

talk_to_google(Url, Re, ResultsIn, Start ) ->
	ResponseData = googleajax:start(Url, Re, Start),

	%% io:format("responseData = ~p ~n", [ResponseData]),
	case ResponseData of
		null -> 
			%% ready
			results_to_struct(ResultsIn, Start-4, nomore);
		_ ->
			%% more data
			ResponseResults=struct:get_value(<<"results">>, ResponseData),
			% scan the results and filter with the regular expression
			Results= lists:flatten([interpret_data(ResponseResults, Re, [] )|ResultsIn]),

			if 
				length(Results) < 20 ->
					_CursorData = struct:get_value(<<"cursor">>, ResponseData),
					talk_to_google(Url, Re, Results, Start+4);
				true ->
					results_to_struct(Results, Start)
			end		
		end.		

results_to_struct(Results, Start) ->
	{struct, [
		{<<"results">>, lists:reverse(Results)},
		{<<"start">>, Start}]}.

results_to_struct(Results, Start, nomore) ->
	struct:set_value( <<"nomore">>, true, 
		results_to_struct(Results, Start)).
	
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

