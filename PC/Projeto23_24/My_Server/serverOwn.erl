-module(serverOwn).
-export([start/0]).
%-import(accounts,[loopContas/1, register_account/2, remove_account/2, login/2, logout/1, getLevel/1, getScore/1, online/0]).

%############################ SERVER START ################################################
start() ->
	startLogin(),
	serverStart().
startLogin() ->
	Contas = spawn(fun() -> loopContas(#{}) end ),
	register(?MODULE, Contas).

serverStart() ->
	Room = spawn(fun() -> waitingRoom(#{}, []) end), 
	{ok, Lsock} = gen_tcp:listen(12345, [binary, {packet, line}, {reuseaddr, true}]),
	spawn(fun() -> acceptor(Lsock, Room) end).

acceptor(Lsock, Room) ->
	try
		{ok, Sock} = gen_tcp:accept(Lsock),
		spawn(fun() -> acceptor(Lsock, Room) end),
		clientParser(Sock, Room, [])
	catch
		error:{badmatch,{error, closed}} ->
			ok
		end.
	

%############################### WAITING ROOM ###################################################

% waitingRoom :: Users -> Salas Warmup -> Msgs
waitingRoom(Users, Warmups) ->
	receive 
		{user_connected, Sock, User} ->
			io:format("Utilizador " ++ User ++ " acabou de entrar! \n"),
			waitingRoom(maps:put(User, Sock, Users), Warmups);

		{user_disconnected, User} ->
			io:format("Utilizador " ++ User ++ " acabou de Sair!\n"),
			waitingRoom(maps:remove(User, Users), Warmups);

		{play, User, Level, {Socket,From,WaitingRoomPid}} -> 
			MatchMaking = matchmaking(Warmups, {User, Level}),
			case MatchMaking of
				null ->  % se recebe null, cria nova sala de Warmup com aquele User e põe a sua média com o nivel do User
					io:format("Creating new warmup room!~n"),
					Warmup = spawn(fun() -> warmup(Level,[{User, Level, Socket,From}]) end),
					Tuple = {Level,[{User,Level,Socket,From}], Warmup}, % {Media,[{Username, Level, Id Socket, Id Warmup room}]}
					NewWarmups = insertWarmup(Tuple,Warmups),
					io:format("~nUser ~p in new room: ~p ~n~n",[User,Warmup]),
					waitingRoom(Users, NewWarmups);
				
				{WarmupRoom} -> % se recebe uma sala, manda mensagem à sala para inserir lá dentro ( se tiver espaço)
					io:format("Here\n"),
					WarmupRoom ! {new_player, {User, Level, Socket, self(),WarmupRoom,WaitingRoomPid}},
					io:format("User entering~n"),
					waitingRoom(Users, Warmups)
				end
		end. 

insertWarmup(Tuple,[]) -> [Tuple];
insertWarmup(Tuple,Warmups) -> [Warmups | Tuple].

% ######################### MATCHMAKING E WARMUP ##########################################

% matchmaking :: [Warmups] -> User -> {Media, Lista}
matchmaking([],_) -> null;
matchmaking([{Media, Lista, WarmupRoom}| _], {_,Level}) when (abs(Media - Level) =< 1) -> 
	{WarmupRoom}; % {Pid Warmup Room, Counter jogadores}
matchmaking(Warmups, User) -> matchmaking(tl(Warmups), User).


% warmup :: Float -> [Users] -> Msgs
warmup(Media, Players) ->
	% qd o warmup começa, começa timer de 5 sec
	receive
		{new_player, {User,Level,Socket,From,WarmupRoom,WaitingRoomPid}}->
			case length(Players) of
				3 ->
					Avg = newAvg(Media, length(Players), Level),
					io:format("~nUser ~p in room ~p ~n~n",[User,WarmupRoom]),
					NewPlayers = Players ++ [{User,Level,Socket,From}],
					io:format("Array jogadores: ~p ~n", [NewPlayers]),
					
					Users = [element(1, Player) || Player <- NewPlayers],
					Levels = [element(2, Player) || Player <- NewPlayers],
					Sockets = [element(3, Player) || Player <- NewPlayers],
					Froms = [element(4, Player) || Player <- NewPlayers],
					
					initGame(Sockets, Users,Froms),
					warmup(0,[]);

				_ ->
					Avg = newAvg(Media, length(Players), Level),
					io:format("~nUser ~p in room ~p ~n~n",[User,WarmupRoom]),
					NewPlayers = Players ++ [{User,Level,Socket,From}],
					io:format("Array jogadores: ~p ~n", [NewPlayers]),
					warmup(Avg, NewPlayers)
				end
	end.


newAvg(Media, N, Level) ->
	((Media * N) + Level) / (N+1).


%############################### ESTADO INICIAL E MOVIMENTOS DE JOGO ######################################

%posx posy angulo fuel
initPlayerState([]) ->
	#{};
initPlayerState([Sock1,Sock2]) ->
	#{Sock1 => {150.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock2 => {1000.0, 600.0, 0.0, 150.0, 0.0, 0.0}};
initPlayerState([Sock1,Sock2,Sock3]) ->
	#{Sock1 => {150.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock2 => {1000.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock3 => {150.0, 1200.0, 0.0, 150.0, 0.0, 0.0}};
initPlayerState([Sock1,Sock2,Sock3,Sock4]) ->
	#{Sock1 => {150.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock2 => {1000.0, 600.0, 0.0, 150.0, 0.0, 0.0}, Sock3 => {150.0, 1200.0, 0.0, 150.0, 0.0, 0.0}, Sock4 => {1000.0, 1200.0, 0.0, 150.0, 0.0, 0.0}}.


initGame(Sockets, Users, Froms) ->
	[gen_tcp:send(Socket, "start\n") || Socket <- Sockets],
	sendPosInfo(Sockets),
	PlayersState = initPlayerState(Sockets),
	MatchPid = spawn(fun() -> createMatch(PlayersState, Users, Sockets)end),
	timer:send_interval(10, MatchPid, {update}),
	[From ! {gamePid, MatchPid} || From <- Froms],
	io:format("Cheguei aqui\n").

sendPosInfo([Socket1,Socket2]) -> % jogador1: x y jogador2: x y
	gen_tcp:send(Socket1, list_to_binary("barPos 50 600 1230 600\n")),
	gen_tcp:send(Socket2, list_to_binary("barPos 1230 600 50 600\n"));
sendPosInfo([Socket1,Socket2,Socket3]) ->
	gen_tcp:send(Socket1, list_to_binary("barPos 50 600 1230 600 50 1300\n")),
	gen_tcp:send(Socket2, list_to_binary("barPos 1230 600 50 600 50 1300\n")),
	gen_tcp:send(Socket3, list_to_binary("barPos 50 1300 50 600 1230 600\n"));
sendPosInfo([Socket1,Socket2,Socket3,Socket4]) ->
	gen_tcp:send(Socket1, list_to_binary("barPos 50 600 1230 600 50 1300 1230 1300\n")), % 1,2,3,4
	gen_tcp:send(Socket2, list_to_binary("barPos 1230 600 50 600 50 1300 1230 1300\n")), % 2,1,3,4
	gen_tcp:send(Socket3, list_to_binary("barPos 50 1300 50 600 1230 600 1230 1300\n")), % 3,1,2,4
	gen_tcp:send(Socket4, list_to_binary("barPos 1230 1300 50 1300 50 600 1230 600\n")). % 4,1,2,3


createMatch(PlayersState, Users, Sockets) ->
	receive
		{key, Key, Sock} ->
			createMatch(movimento(Key, PlayersState, Sock), Users, Sockets);

		{exited} ->
			[gen_tcp:send(Socket, list_to_binary("end\n")) || Socket <- Sockets];

		{colision, Player, Food} ->
			io:format("Colisão");
		{update} ->
			Socket1 = lists:nth(1,Sockets), Socket2 = lists:nth(2,Sockets),
			gen_tcp:send(Socket1, list_to_binary("playerUpdate " ++ handlePlayer(Socket1, PlayersState) ++ " " ++ handlePlayer(Socket2, PlayersState) ++ "\n")),
			gen_tcp:send(Socket2, list_to_binary("playerUpdate " ++ handlePlayer(Socket2, PlayersState) ++  " " ++ handlePlayer(Socket1, PlayersState) ++ "\n")),
			io:format("update\n"),
			createMatch(PlayersState,Users, Sockets)

		%{update} ->
			%checkIntersections({Socket, PlayersState}, maps:to_list(FoodGreen), self()),
			%checkIntersections({Socket2, PlayersState}, maps:to_list(FoodGreen), self()),
			%gen_tcp:send(Socket, list_to_binary("playerUpdate " ++ handlePlayer(Socket, PlayersState) ++ " " ++ handlePlayer(Socket2, PlayersState) ++ "\n")),
			%gen_tcp:send(Socket2, list_to_binary("playerUpdate " ++ handlePlayer(Socket2, PlayersState) ++  " " ++ handlePlayer(Socket, PlayersState) ++ "\n")),
			%gen_tcp:send(Socket, list_to_binary("foodGreenPos " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n")),
			%gen_tcp:send(Socket2, list_to_binary("foodGreenPos " ++ handleFoodGreen("fg1", FoodGreen) ++  " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n")),
			%gen_tcp:send(Socket, list_to_binary("foodRedPos " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n")),
			%gen_tcp:send(Socket2, list_to_binary("foodRedPos " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n")),
			%createMatch(updateFoodGreen(maps:to_list(FoodGreen), maps:to_list(PlayersState)), array:from_list(updateFoodRed(array:to_list(FoodRed), maps:to_list(PlayersState))), PlayersState, User, User2, Socket, Socket2)

	end.


handlePlayer(Sock, PlayersState) ->
	case maps:get(Sock, PlayersState) of
		{X,Y, Alpha, Fuel, _, _} ->
			(float_to_list(X,[{decimals,1}]) ++ " " ++ float_to_list(Y,[{decimals,1}]) ++ " " ++ 
				float_to_list(Alpha,[{decimals,1}]) ++ " " ++ float_to_list(Fuel,[{decimals,1}]));
		_ -> true
	end.

initJogo(Socket, Socket2, User, User2, From, From2) ->
		gen_tcp:send(Socket2, "start\n"),
		gen_tcp:send(Socket, "start\n"),
		%PlayersState = initializePlayersState(Socket, Socket2),
		gen_tcp:send(Socket, list_to_binary("barPos 50 600 1230 600\n")),
		gen_tcp:send(Socket2, list_to_binary("barPos 1230 600 50 600\n")),
		FoodGreen = initFoodGreen(),
		FoodRed = initFoodRed(array:new({default, 0})),
		%MatchPid = spawn(fun() -> createMatch(FoodGreen, FoodRed, PlayersState, User, User2, Socket, Socket2) end),
		%timer:send_interval(10, MatchPid, {update}),
		%timer:send_interval(10000, MatchPid, {foodRed}),
		%From  ! {gamePid, MatchPid},
		%From2 ! {gamePid, MatchPid},
		gen_tcp:send(Socket, "createFoodGreen " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n"),
		gen_tcp:send(Socket2, "createFoodGreen " ++ handleFoodGreen("fg1", FoodGreen) ++ " " ++ handleFoodGreen("fg2", FoodGreen) ++ "\n"),
		gen_tcp:send(Socket, "createFoodRed " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n"),
		gen_tcp:send(Socket2, "createFoodRed " ++ handleFoodRed(array:to_list(FoodRed)) ++ " \n").





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
	#{"fg1" => {rand:uniform(1000) * 1.0, rand:uniform(600) * 1.0}, "fg2" => {rand:uniform(1000) * 1.0, rand:uniform(600) * 1.0}}.

initFoodRed(FoodRed) ->
	array:set(array:size(FoodRed), {rand:uniform(1000) * 1.0, rand:uniform(600) * 1.0}, FoodRed).

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
	array:set(array:size(FoodRed), {rand:uniform(1000) * 1.0, rand:uniform(600) * 1.0}, FoodRed).

foodEaten(Food, FoodGreen) ->
	maps:update(Food, {rand:uniform(1000) * 1.0, rand:uniform(600) * 1.0}, FoodGreen).

fillFuel(Player, Players) ->
	case maps:get(Player, Players) of
		{X, Y, A, _, V, Acc} ->
			maps:update(Player, {X, Y, A, 150.0, V, Acc}, Players);

		_ -> true
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
					Room ! {play, lists:nth(2, Recebido), getLevel(lists:nth(2, Recebido)), {Socket, self(),Room}},
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
			clientParser(Socket, Room, Match);
		
		{tcp_closed, Socket} ->
            % Socket closed unexpectedly, terminate the process
            io:format("Socket closed unexpectedly. Terminating process.~n"),
            exit(normal);

        {error, Socket, Reason} ->
            % Error occurred with the socket, handle it accordingly
            io:format("Socket error: ~p. Terminating process.~n", [Reason]),
            exit(normal)
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