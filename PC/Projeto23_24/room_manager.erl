-module(rm).
-export([create_room/1,
        join_room/1,
        leave_room/1]).

% Lista de salas %
-define(Rooms, []).

% Cria uma nova sala %
create_room(MaxPlayers) ->
    RoomId = generate_room(),
    Room = #room{id = RoomId, players = [], max_players = MaxPlayers},
    NewRooms = [Room | ?Rooms],
    {ok, RoomId, NewRooms}.

% Insere um jogador numa sala existente %
join_room(PlayerId) ->
    case room_available() of
        {ok, RoomId} ->
            NewRooms = add_player(RoomId, PlayerId),
            {ok, RoomId, NewRooms};
        {error, Reason} ->
            {error, Reason}
    end.

% Retira jogador de uma sala %
leave_room(PlayerId) ->
    case find_player(PlayerId) of
        {ok, RoomId} ->
            NewRooms = remove_player(RoomId, PlayerId),
            {ok, NewRooms};
        {error, Reason} ->
            {ok, Reason}
    end.


% AUXILIARES %

- record(state, {room_counter = 0}).

generate_room() ->
    % Implemente uma lógica para gerar IDs de sala únicos
    % Pode ser um número sequencial, um UUID, etc.
    
    #state{room_counter = Counter} = get_state(),
    NewCounter = Counter + 1,
    set_state(#state{room_counter = NewCounter}),
    NewCounter.

find_room() ->
    % Implemente a lógica para encontrar uma sala com espaço disponível
    Counter = 0,
    NewCounter = Counter +1,
    ?MODULE ! {ok,NewCounter}
.

add_player(RoomId, PlayerId) ->
    % Implemente a lógica para adicionar um jogador a uma sala
    .

remove_player(RoomId, PlayerId) ->
    % Implemente a lógica para remover um jogador de uma sala
    .

find_player(PlayerId) ->
    % Implemente a lógica para encontrar a sala em que um jogador está
    .