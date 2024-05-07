-module(accounts).
-export([loopContas/1, register_account/2, remove_account/2, login/2, logout/1, getLevel/1, getScore/1, online/0]).

loopContas(Map) ->
	receive
		{{register, User, Pass}, From} ->
			case maps:is_key(User, Map) of
			 	true -> From ! {?MODULE, error}, loopContas(Map);
			 	false -> From ! {?MODULE, ok}, loopContas(maps:put(User,{Pass, offline, 0, 0},Map))
			end;
		{{remove, User, Pass}, From} ->
			case maps:find(User, Map) of
				{ok, {Pass, _, _, _}} -> From ! {?MODULE, ok},loopContas(maps:remove(User, Map));
				_ -> From ! {?MODULE, error},loopContas(Map)
			end;
		{{login, User, Pass}, From} ->
			case maps:find(User, Map) of
				{ok, {Pass, offline, Level, TopScore}} -> From ! {?MODULE, ok}, loopContas(maps:update(User,{Pass, online, Level, TopScore},Map));
				_ -> From ! {?MODULE, error}, loopContas(Map)
			end;
		{{logout, User}, From} ->
			case maps:find(User, Map) of
				{ok, {Pass, online, Level, TopScore}} -> From ! {?MODULE, ok}, loopContas(maps:update(User,{Pass, offline, Level, TopScore},Map));
				_ -> From ! {?MODULE, error}, loopContas(Map)
			end;
		{{getlevel, User}, From} ->
			case maps:find(User, Map) of
				{ok, {_, _, Level, _}} -> From ! {?MODULE, Level}, loopContas(Map);
				_ -> From ! {?MODULE, error}, loopContas(Map)
			end;
		{{getscore, User}, From} ->
			case maps:find(User, Map) of
				{ok, {_, _, _, TopScore}} -> From ! {?MODULE, TopScore}, loopContas(Map);
				_ -> From ! {?MODULE, error}, loopContas(Map)
			end;
		{{online}, From} ->
			L = lists:map(fun({X, _}) -> X end, lists:filter(fun({_, {_, A, _, _}}) ->
				A == online end, lists:map(fun(X) ->
					{X,maps:get(X, Map)} end, maps:keys(Map)))),
			From !{?MODULE, L} , loopContas(Map)
	end.

% Funções de operações de contas %

request(Msg) ->
	?MODULE ! {Msg, self()},
	receive
		{?MODULE, Tentativa} -> Tentativa
	end.


register_account(Username, Pass) ->
	request({register, Username, Pass}).

remove_account(Username, Pass) ->
	request({remove, Username, Pass}).

login(Username, Pass) -> 
	request({login, Username, Pass}).

logout(Username) -> 
	request({logout, Username}).


getLevel(Username) -> 
	request({getlevel, Username}).

getScore(Username) -> 
	request({getscore, Username}).

online() ->
	request({online}).