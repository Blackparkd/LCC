-module(servidor).
-export([start/0]).

start()->
    startRm(),
    startLm().

startRm()->
    RmPid = spawn(fun()-> rm(#{},#{}) end).

startLm()->
    LmPid = spawn(fun() -> lm(#{}) end).

rm(Users, Rooms)->
    receive 
		{user_connected, Sock, User} ->
			io:format("User " ++ User ++ " connected! \n"),
			rm(maps:put(User, Sock, Users), Rooms);

		{user_disconnected, User} ->
			io:format("User " ++ User ++ " disconnected!\n"),
			rm(maps:remove(User, Users), Rooms);

		{make, User, Level, {Socket,From}} ->
            Players = [User],
			case matchmaking(1,Level, maps:to_list(Rooms)) of
				[Num,User2, Level2, Socket2, From2] ->
                    lists:append([User2],Players),
                    case checkTiming() of
                        true ->
                            From ! {play, Players,{Socket, From}};
					
				[] -> 
					gen_tcp:send(Socket, "Wait\n"),
					rm(Users, maps:put(Level, {User, Socket, From}, Rooms)); % se o bestmatch não devolver nada, põe o User em espera (ReadyToPlayUsers)
        
        {play, Players, {Socket, From}} ->
            io:format("Game starting: " ++ Num ++ " users playing!\n "),
			initJogo(Players), % começa o jogo com a lista dos jogadores
            rm(Users, maps:remove(Level2, Rooms))
		end
	end.

matchmaking(4,_,[{_, {User,Socket,From}}]) ->
    From ! {"Maximum occupancy: Room Closed\n"};

matchmaking(Num, Level1, [{Level2, {User, Socket, From}} | _]) when ((Level1 == Level2) or (Level1 == Level2 + 1) or (Level1 == Level2 - 1)) ->
    [Num+1,User, Level2, Socket, From].

checkTiming() ->
    % Função temporizador de começar o jogo
    receive
        {Time} ->
            Time
        end.