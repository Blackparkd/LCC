-module(server).
-export([start/1]).

% Funções de operações de contas %
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

% Funções de arranque de servidor %
start(Port) ->
	startLogin(),
	serverStart(Port).

serverStart(Port) ->	% #{} -> AllUsers	#{} -> WaitingUsers
	Room = spawn(fun() -> waitingRoom(#{}, #{}) end), 
	{ok, Lsock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
	spawn(fun() -> acceptor(Lsock, Room) end).

acceptor(Lsock, Room) ->
	{ok, Sock} = gen_tcp:accept(Lsock),
	spawn(fun() -> acceptor(Lsock, Room) end),
	clientParser(Sock, Room, []).

startLogin() ->
	Pid = spawn(fun() -> loopContas(#{}) end ),
	register(?MODULE, Pid).

request(Msg) ->
	?MODULE ! {Msg, self()},
	receive
		{?MODULE, Tentativa} -> Tentativa
	end.

% bestmatch recebe um certo level e uma lista de tuplos : (level2,(user,socket, origem))
bestmatch(_, []) -> [];
bestmatch(Level, [{Level2, {User, Socket, From}} | _]) when ((Level == Level2) or (Level == Level2 + 1) or (Level == Level2 - 1)) ->
	[User, Level2, Socket, From];
bestmatch(Level, LvlUser) -> 
	bestmatch(Level, tl(LvlUser)). % tl -> função para aceder à tail da lista LvlUser neste caso

initJogo(Socket, Socket2, User, User2, From, From2) ->
		gen_tcp:send(Socket2, "start\n"),
		gen_tcp:send(Socket, "start\n"),
		PlayersState = initializePlayersState(Socket, Socket2),
		gen_tcp:send(Socket, list_to_binary("barPos 50 600 1230 600\n")),
		gen_tcp:send(Socket2, list_to_binary("barPos 1230 600 50 600\n")),
		FoodGreen = initFoodGreen(),
		FoodRed = initFoodRed(array:new({default, 0})),
		MatchPid = spawn(fun() -> createMatch(FoodGreen, FoodRed, PlayersState, User, User2, Socket, Socket2) end),
		timer:send_interval(10, MatchPid, {update}),
		timer:send_interval(10000, MatchPid, {foodRed}),
		From  ! {gamePid, MatchPid},
		From2 ! {gamePid, MatchPid},
		gen_tcp:send(Socket, "createFoodGreen " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n"),
		gen_tcp:send(Socket2, "createFoodGreen " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n"),
		gen_tcp:send(Socket, "createFoodRed " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n"),
		gen_tcp:send(Socket2, "createFoodRed " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n").

% waitingRoom recebe dois maps: Users existentes e Users prontos para o jogo (depois de avaliados por bestmatch)
waitingRoom(Users, ReadyToPlayUsers) ->
	receive 
		{user_connected, Sock, User} ->
			io:format("Utilizador " ++ User ++ " acabou de entrar! \n"),
			waitingRoom(maps:put(User, Sock, Users), ReadyToPlayUsers);

		{user_disconnected, User} ->
			io:format("Utilizador " ++ User ++ " acabou de Sair!\n"),
			waitingRoom(maps:remove(User, Users), ReadyToPlayUsers);

		{play, Players, User, Level, {Socket,From}} ->
			case length(Players) of

				4 -> initJogo(SocketList, PlayersList, FromList),
				waitingRoom(Users, ReadyToPlayUsers);
				
				1 -> 
					case bestmatch(Level, maps:to_list(ReadyToPlayUsers)) of
						[User2, Level2, Socket2, From2] ->
							[{User2,Level2,Socket2,From2}] ++ Players,
							From ! {play, Players, User, Level, {Socket, From}},
							waitingRoom(Users, maps:remove(Level2, ReadyToPlayUsers));
						[] -> 
							gen_tcp:send(Socket, "Wait\n"),
							waitingRoom(Users, maps:put(Level, {User, Socket, From}, ReadyToPlayUsers)) % se o bestmatch não devolver nada, põe o User em espera (ReadyToPlayUsers)
					end;
				_ ->
					% start timer 5 sec para começar
					case bestmatch(Level, maps:to_list(ReadyToPlayUsers)) of
						[User2, Level2, Socket2, From2] ->
							[{User2,Level2,Socket2,From2}] ++ Players,
							From ! {play, Players, User, Level, {Socket, From}},
							waitingRoom(Users, maps:remove(Level2, ReadyToPlayUsers));
						[] -> 
							gen_tcp:send(Socket, "wait\n"),
							waitingRoom(Users, maps:put(Level, {User, Socket, From}, ReadyToPlayUsers)) % se o bestmatch não devolver nada, põe o User em espera (ReadyToPlayUsers)
				end
					
			end
		end.



%posx posy angulo fuel
initializePlayersState(Sock, Sock2) ->
	#{Sock => {150.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock2 => {1000.0, 600.0, 0.0, 150.0, 0.0, 0.0}}.

handlePlayer(Sock, PlayersState) ->
	case maps:get(Sock, PlayersState) of
		{X,Y, Alpha, Fuel, _, _} ->
			(float_to_list(X,[{decimals,1}]) ++ " " ++ float_to_list(Y,[{decimals,1}]) ++ " " ++ 
				float_to_list(Alpha,[{decimals,1}]) ++ " " ++ float_to_list(Fuel,[{decimals,1}]));
		_ -> true
	end.

incMovimento(Sock, PlayersState) ->
	case maps:get(Sock, PlayersState) of
		{X,Y, Alpha, Fuel, Vel, Acc} ->
			if 
				Fuel-1 >= 0 ->
					maps:update(Sock, {X + math:sin(Alpha) * 5, Y - math:cos(Alpha) * 5, Alpha, Fuel-1, Vel, Acc}, PlayersState);

				true ->
					PlayersState
			end;
		_ -> PlayersState
	end.

rotateRight(Sock, PlayersState) ->
	case maps:get(Sock, PlayersState) of
		{X,Y, Alpha, Fuel, Vel, Acc} ->
			if 
				Fuel-1 >= 0 ->
					maps:update(Sock, {X, Y, Alpha + 0.1, Fuel, Vel, Acc}, PlayersState);

				true ->
					PlayersState
			end;
		_ -> PlayersState
	end.

rotateLeft(Sock, PlayersState) ->
	case maps:get(Sock, PlayersState) of
		{X,Y, Alpha, Fuel, Vel, Acc} ->
			if 
				Fuel-1 >= 0 ->
					maps:update(Sock, {X, Y, Alpha - 0.1, Fuel, Vel, Acc}, PlayersState);

				true ->
					PlayersState
			end;
		_ -> PlayersState
	end.

movimento(Key, PlayersState, Sock) ->
	case Key of
		"front" ->
			incMovimento(Sock, PlayersState);
		"right" ->
			rotateRight(Sock, PlayersState);
		"left" ->
			rotateLeft(Sock, PlayersState)
	end.


%tem de ser sempre dois
initFoodGreen() ->
	#{"fg1" => {random:uniform(1000) * 1.0, random:uniform(600) * 1.0}, "fg2" => {random:uniform(1000) * 1.0, random:uniform(600) * 1.0}}.

initFoodRed(FoodRed) ->
	array:set(array:size(FoodRed), {random:uniform(1000) * 1.0, random:uniform(600) * 1.0}, FoodRed).

distance({X, Y}, {PX, PY}) ->
	(math:sqrt(math:pow(PX - X, 2) + math:pow(PY - Y, 2))).

checkDistFood({X, Y}, {P1X, P1Y}, {P2X, P2Y}) ->
	case distance({X, Y}, {P1X, P1Y}) > distance({X, Y}, {P2X, P2Y}) of
		true ->
			{X + ((P2X - X) * 0.7)/distance({X, Y}, {P2X, P2Y}), Y + ((P2Y - Y) * 0.7)/distance({X, Y}, {P2X, P2Y})};
		false ->
			{X + ((P1X - X) * 0.7)/distance({X, Y}, {P1X, P1Y}), Y + ((P1Y - Y) * 0.7)/distance({X, Y}, {P1X, P1Y})}

	end.

checkIntersection({X1, Y1}, {X2, Y2}) ->
	distance({X1, Y1}, {X2, Y2}) < 50.

getPosPlayer(Player, Players) ->
	case maps:get(Player, Players) of
		{X, Y, _, _, _, _} ->
			{X, Y};
		_ -> 
			true
	end.

checkIntersections({Player, Players}, [{G, {GX, GY}}, {G2, {GX2, GY2}}], MatchPid) ->
	case checkIntersection(getPosPlayer(Player, Players), {GX, GY}) of
		true ->
			MatchPid ! {colision, Player, G};
		_ -> case checkIntersection(getPosPlayer(Player, Players), {GX2, GY2}) of
			true -> 
				MatchPid ! {colision, Player, G2};
			_ ->
				true
			end
	end.


updateFoodGreen([{A, {X, Y}}, {B, {X2, Y2}}], [{_, {P1X, P1Y, _, _, _, _}}, {_, {P2X, P2Y, _, _, _, _}}]) ->
	#{A => checkDistFood({X, Y}, {P1X, P1Y}, {P2X, P2Y}), B => checkDistFood({X2, Y2}, {P1X, P1Y}, {P2X, P2Y})}.

handleFoodGreen(F, FoodGreen) ->
	case maps:get(F, FoodGreen) of
		{X,Y} ->
			(float_to_list(X,[{decimals,1}]) ++ " " ++ float_to_list(Y,[{decimals,1}]));
		_ -> true
	end.

handleFoodRed([]) -> " ";
handleFoodRed([{X, Y} | T]) ->
	(float_to_list(X,[{decimals,1}]) ++ " " ++ float_to_list(Y,[{decimals,1}]))++ " " ++ handleFoodRed(T).


updateFoodRed([{X, Y}], [{_, {P1X, P1Y, _, _, _, _}}, {_, {P2X, P2Y, _, _, _, _}}]) ->
	[checkDistFood({X, Y}, {P1X, P1Y}, {P2X, P2Y})];
updateFoodRed([{X, Y} | T], [{P1, {P1X, P1Y, A1, F1, Vel1, Acc1}}, {P2, {P2X, P2Y, A2, F2, Vel2, Acc2}}]) ->
	[checkDistFood({X, Y}, {P1X, P1Y}, {P2X, P2Y})] ++ updateFoodRed(T, [{P1, {P1X, P1Y, A1, F1, Vel1, Acc1}}, {P2, {P2X, P2Y, A2, F2, Vel2, Acc2}}]).

createFoodRed(FoodRed) ->
	array:set(array:size(FoodRed), {random:uniform(1000) * 1.0, random:uniform(600) * 1.0}, FoodRed).

foodEaten(Food, FoodGreen) ->
	maps:update(Food, {random:uniform(1000) * 1.0, random:uniform(600) * 1.0}, FoodGreen).

fillFuel(Player, Players) ->
	case maps:get(Player, Players) of
		{X, Y, A, _, V, Acc} ->
			maps:update(Player, {X, Y, A, 150.0, V, Acc}, Players);

		_ -> true
	end.

%% PROCESSO NOVO PARA CADA SIMULACAO
createMatch(FoodGreen, FoodRed, PlayersState, User, User2, Socket, Socket2) ->
	receive
		%{stateUpdate, NewState} ->
		%	createMatch(FoodGreen, NewState, User, User2, Socket, Socket2);
		{foodRed} ->
			%io:format("nova food red~n"),
			createMatch(FoodGreen, createFoodRed(FoodRed), PlayersState, User, User2, Socket, Socket2);

		{key, Key, Sock} ->
			createMatch(FoodGreen, FoodRed, movimento(Key, PlayersState, Sock), User, User2, Socket, Socket2);

		{exited} ->
			gen_tcp:send(Socket, list_to_binary("end\n")),
			gen_tcp:send(Socket2, list_to_binary("end\n"));

		{colision, Player, Food} ->
			createMatch(foodEaten(Food, FoodGreen), FoodRed, fillFuel(Player, PlayersState), User, User2, Socket, Socket2);

		{update} ->
			checkIntersections({Socket, PlayersState}, maps:to_list(FoodGreen), self()),
			checkIntersections({Socket2, PlayersState}, maps:to_list(FoodGreen), self()),
			gen_tcp:send(Socket, list_to_binary("playerUpdate " ++ handlePlayer(Socket, PlayersState) ++ " " ++ handlePlayer(Socket2, PlayersState) ++ "\n")),
			gen_tcp:send(Socket2, list_to_binary("playerUpdate " ++ handlePlayer(Socket2, PlayersState) ++  " " ++ handlePlayer(Socket, PlayersState) ++ "\n")),
			gen_tcp:send(Socket, list_to_binary("foodGreenPos " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n")),
			gen_tcp:send(Socket2, list_to_binary("foodGreenPos " ++ handleFoodGreen("fg1", FoodGreen) ++  " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n")),
			gen_tcp:send(Socket, list_to_binary("foodRedPos " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n")),
			gen_tcp:send(Socket2, list_to_binary("foodRedPos " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n")),
			createMatch(updateFoodGreen(maps:to_list(FoodGreen), maps:to_list(PlayersState)), array:from_list(updateFoodRed(array:to_list(FoodRed), maps:to_list(PlayersState))), PlayersState, User, User2, Socket, Socket2)

	end.



clientParser(Socket, Room, MatchPid) ->
	receive
		{tcp, _, Data} ->
			Recebido = string:tokens(binary_to_list(Data), "\r\n "),
			Tipo = lists:nth(1, Recebido),
			case Tipo of
				"login" -> 
					Tentativa = login(lists:nth(2, Recebido), lists:nth(3, Recebido)),
					case Tentativa of 
						ok ->
							gen_tcp:send(Socket, "ok2\n"),
							Room ! {user_connected, Socket, lists:nth(2, Recebido)},
							clientParser(Socket, Room, MatchPid);
						error ->
							gen_tcp:send(Socket, "error\n"),
							clientParser(Socket, Room, MatchPid)
					end;

				"logout" -> 
					Tentativa = logout(lists:nth(2, Recebido)),
					case Tentativa of
						ok ->
							gen_tcp:send(Socket, "ok\n"),
							Room ! {user_disconnected, lists:nth(2, Recebido)},
							clientParser(Socket, Room, MatchPid);
						error ->
							gen_tcp:send(Socket, "error!\n"),
							clientParser(Socket, Room, MatchPid)
					end;

				"register" -> 
					Tentativa = register_account(lists:nth(2, Recebido), lists:nth(3, Recebido)),
					case Tentativa of
						ok ->
							gen_tcp:send(Socket, "ok1\n"),
							clientParser(Socket, Room, MatchPid);
						error ->
							gen_tcp:send(Socket, "error\n"),
							clientParser(Socket, Room, MatchPid)
					end;

				"remove" -> 
					Tentativa = remove_account(lists:nth(2, Recebido), lists:nth(3, Recebido)),
					case Tentativa of
						ok ->
							gen_tcp:send(Socket, "ok\n"),
							Room ! {user_disconnected, Socket, lists:nth(2, Recebido)},
							clientParser(Socket, Room, MatchPid);
						error ->
							gen_tcp:send(Socket, "error\n"),
							clientParser(Socket, Room, MatchPid)
					end;

				"key" ->
					%%trocar ultimo por user
					MatchPid ! {key, lists:nth(2, Recebido), Socket},
					clientParser(Socket, Room, MatchPid);

				"play" ->
					Room ! {play, lists:nth(2, Recebido), getLevel(lists:nth(2, Recebido)), {Socket, self()}},
					clientParser(Socket, Room, MatchPid);

				"frame" ->
					MatchPid ! {frame},
					clientParser(Socket, Room, MatchPid);

				"exited" ->
					MatchPid ! {exited},
					clientParser(Socket, Room, MatchPid);
				_->
					true


			end;

		{gamePid, Match} ->
			clientParser(Socket, Room, Match)


	end.


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

	
