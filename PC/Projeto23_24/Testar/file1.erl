-module(file1).
-export([imprimir/0]).

imprimir() ->
    io:fwrite("ficheiro ~w ~n",[34]).

