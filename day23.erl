-module(day23).
-compile(export_all).

at(T, Pos) ->
    case ets:lookup(T, Pos) of
        [{Pos, Val}] -> Val;
        _ -> $. %% outside range is all free
    end.

parse_map_line(Bin, Y, T) ->
    [Line | Rest] = binary:split(Bin, <<"\n">>),
    [ ets:insert(T, {{X,Y},At}) || {X,At} <- [ {X+1, binary:at(Line, X)} || X <- lists:seq(0, size(Line)-1) ],
                                 At =/= 32 ],
    case Rest of
        [] -> T;
        [<<"\n">>] -> T;
        [RestBin] -> parse_map_line(RestBin, Y+1, T)
    end.

dims(T) ->
    ets:foldl(fun({{X,Y},_}, none) -> {X,X,Y,Y};
                 ({{X,Y},_}, {MinX, MaxX, MinY, MaxY}) ->
                      {min(MinX,X), max(X,MaxX), min(Y,MinY), max(Y,MaxY)}
              end, none, T).
dims(T, Containing) ->
    ets:foldl(fun({_,Ch}, V) when Ch =/= Containing -> V;
                 ({{X,Y},_}, none) -> {X,X,Y,Y};
                 ({{X,Y},_}, {MinX, MaxX, MinY, MaxY}) ->
                      {min(MinX,X), max(X,MaxX), min(Y,MinY), max(Y,MaxY)}
              end, none, T).

parse_map(Bin) ->
    %% All lines of the map are not the same width, we need to take the max
    T = ets:new(day22, [set]),
    parse_map_line(Bin, 1, T).

input(File) ->
    {ok, Bin} = file:read_file(File),
    parse_map(Bin).

input() -> input("day23.txt").
sample() -> input("day23_sample.txt").

-define(MOVES, [[{0,-1},{-1,-1},{1,-1}], %% move N if N,NW,NE are free
                [{0,1},{-1,1},{1,1}], %% move S if S,SW,SE are free
                [{-1,0},{-1,-1},{-1,1}], %% move W if W,NW,SW are free
                [{1,0},{1,-1},{1,1}]]). %% move E if E,NE,SE are free

free(M, Pos) ->
    case at(M, Pos) of
        $. -> true;
        $# -> false
    end.

propose(M, {X,Y}, Moves) ->
    case alone(M, {X,Y}) of
        true -> none;
        false ->
            case [ Move || Move <- Moves,
                           lists:all(fun({Dx,Dy}) -> free(M, {X+Dx,Y+Dy}) end, Move) ] of
                [ [{Dx,Dy}| _ ] | _] -> {X+Dx,Y+Dy};
                _ -> none
            end
    end.

alone(M, {X,Y}) ->
    %% Am I alone here? (no one around me)
    lists:all(fun({Dx,Dy}) -> free(M,{X+Dx,Y+Dy}) end,
              [{-1,-1}, {0,-1},{1,-1},
               {-1,0}, {1,0},
               {-1,1}, {0,1}, {1,1}]).


play_round(M, Moves) ->
    ProposedMoves = ets:foldl(fun({_,$.}, P) -> P;
                                 ({Pos,$#}, P) ->
                                      case propose(M, Pos, Moves) of
                                          none -> P;
                                          MoveTo -> maps:update_with(MoveTo, fun(X) -> [Pos | X] end,
                                                                     [Pos], P)
                                      end
                              end, #{}, M),
    %% Execute moves to unique positions
    maps:fold(fun(To, [From], Acc) ->
                      %% Exactly one Elf wants to move here, do oit
                      %%ets:delete(M, From),
                      ets:insert(M, {From,$.}),
                      ets:insert(M, {To, $#}),
                      Acc + 1;
                 (_,_,Acc) -> Acc
              end,
              0, ProposedMoves).

print(T, {MinX,MaxX,_,_}=D, X, Y) when X > MaxX ->
    io:format("~n",[]),
    print(T,D, MinX, Y+1);
print(_, {_,_,_,MaxY}, _, Y) when Y > MaxY -> ok;
print(T, D, X, Y) ->
    io:format("~c", [at(T,{X,Y})]),
    print(T, D, X+1, Y).


print(T) ->
    {MinX,MaxX,MinY,MaxY} = dims(T),
    D = {MinX-4,MaxX+4,MinY-2,MaxY+2},
    print(T, D, MinX-4, MinY-2).

count_space(T) ->
    {MinX,MaxX,MinY,MaxY} = dims(T, $#),
    lists:sum(
      [ case at(T,{X,Y}) of
            $. -> 1;
            _ -> 0
        end || X <- lists:seq(MinX,MaxX),
               Y <- lists:seq(MinY,MaxY) ]).

rotate(Moves) -> tl(Moves) ++ [hd(Moves)].

part1() ->
    T = input(),
    lists:foldl(fun(_,Moves) ->
                        %%print(T), io:format("-------~n",[]),
                        play_round(T, Moves),
                        rotate(Moves)
                end,
                ?MOVES, lists:seq(1,10)),
    count_space(T).

part2() ->
    T = input(),
    part2(T, ?MOVES, 1).

part2(T, Moves, Round) ->
    case play_round(T, Moves) of
        0 -> Round;
        _ -> part2(T, rotate(Moves), Round+1)
    end.
