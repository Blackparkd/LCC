-module(chatv2).
-export([start/1, stop/1]).

% erlc chatv2.erl para compilar
% erl para entrar no interpretador
% chatv2: <função> (<args>) para correr

start(Port) -> spawn(fun() -> server(Port) end).
stop(Server) -> Server ! stop.

server(Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
	register(rm, spawn(fun() -> rm(#{}) end)),
	%Room = spawn(fun()-> room([]) end),
	spawn(fun() -> acceptor(LSock) end),
	receive stop -> ok end.

acceptor(LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	spawn(fun() -> acceptor(LSock) end),
	RoomPid = getRoom("geral"),
	RoomPid ! {enter, self()},
	user(Sock,RoomPid).


% ROOM MANAGER %

getRoom(Name) ->	% get vai ser invocado pelo user %
	rm ! {get, Name, self()},
	receive {RoomPid, rm} -> RoomPid % recebe o roompid do rm e devolve Roompid
end.

rm(Rooms) ->
	receive
		{getRoom, Name, From} ->
			case maps:find(Name, Rooms) of
				% o find retorna{ok, value} ou error %
				{ok, RoomPid} ->
					From ! {RoomPid, rm},
					rm(Rooms);

				error ->
					RoomPid = spawn(fun() -> room([]) end),
					From ! {RoomPid, rm},
					rm(maps:put(Name, RoomPid, Rooms))
				end
			end.

room(Pids) ->
	receive
		{enter, Pid} ->
			io:format("User entered~n", []),
			room([Pid | Pids]);

		{line, Data} = Msg ->
			io:format("Received ~p~n", [Data]),
			[Pid ! Msg || Pid <- Pids],
			room(Pids);
		
		{leave, Pid} ->
			io:format("User left~n", []),
			room(Pids -- [Pid]),
			Pid ! {left, self()}
	end.

% %

% USER %

user(Sock,Room) ->
	receive
		{line, Data} ->
			gen_tcp:send(Sock, Data),
			user(Sock, Room);
		
		{tcp,_,"/Room " ++ Name} ->
			Room ! {leave, self()},
			% criação de um loop até não haver msg da outra sala %
			NewRoom = getRoom(Name),
			NewRoom ! {enter, self()},
			user(Sock, NewRoom);

		{tcp, _, Data} ->
			Room ! {line, Data},
			user(Sock, Room);
		
		{tcp_closed, _} ->
			Room ! {leave, self()};
		
		{tcp_error, _, _} ->
			Room ! {leave, self()}
	end.







%leaving(Pid, Socket) ->
%	receive
%		{left,Pid} -> ok;
%		{data, Pid} ->
			
% Pid = roommanager: get(Sala)
% Pid ! {enter, self()}
% User(Socket, Room(?),Pid)

%get(Room) ->
%	room ! {get,Room,self()},
%	receive
%		{Res, Room} -> res 
%	end.
% loop(rooms) ->
%	receive ->
%	
%	end