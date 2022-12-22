-module(day22).
-compile(export_all).
-record(state, {%% 2D grid of the map {W, H, Table}
                map :: {integer(), integer(), reference()},
                 %% current player position
                pos :: {integer(), integer()},
                %% direction currently facing  0=right,1=down,2=left,3=up
                face = 0 :: integer(),
                %% The directions  list of Count or turn
                directions
               }).


parse_map_line(Bin, Y, T) ->
    [Line | Rest] = binary:split(Bin, <<"\n">>),
    [ ets:insert(T, {{X,Y},At}) || {X,At} <- [ {X+1, binary:at(Line, X)} || X <- lists:seq(0, size(Line)-1) ],
                                 At =/= 32 ],
    case Rest of
        [] -> done;
        [<<"\n">>] -> done;
        [RestBin] -> parse_map_line(RestBin, Y+1, T)
    end.

dims(T) ->
    ets:foldl(fun({{X,Y},_}, {W,H}) ->
                      {case W of
                           undefined -> X;
                           _ -> max(W,X) end,
                       case H of
                           undefined -> Y;
                           _ -> max(H,Y) end}
              end, {undefined, undefined}, T).


parse_map(Bin) ->
    %% All lines of the map are not the same width, we need to take the max
    T = ets:new(day22, [set]),
    parse_map_line(Bin, 1, T),
    {W,H} = dims(T),
    {W,H,T}.

parse_dirs(<<"\n">>, Num, Acc) ->
    [ I || I <- lists:reverse([Num | Acc]), I =/= 0 ];
parse_dirs(<<"L", Rest/binary>>, Num, Acc) ->
    parse_dirs(Rest, 0, ["L", Num | Acc]);
parse_dirs(<<"R", Rest/binary>>, Num, Acc) ->
    parse_dirs(Rest, 0, ["R", Num | Acc]);
parse_dirs(<<Digit:8, Rest/binary>>, Num, Acc) ->
    parse_dirs(Rest, (Num * 10) + (Digit - 48), Acc).

at({W,H,_}, {X,Y}) when X < 1 orelse X > W orelse Y < 1 orelse Y > H -> nothing;
at({_,_,T}, Pos) ->
    case ets:lookup(T, Pos) of
        [{Pos, Val}] -> Val;
        _ -> nothing
    end.

starting_pos({W,_,_}=M, X, Y) when X > W -> starting_pos(M, 1, Y+1);
starting_pos(M, X, Y) ->
    case at(M, {X,Y}) of
        $. -> {X,Y};
        _ -> starting_pos(M, X+1, Y)
    end.

input(F) ->
    {ok, Bin} = file:read_file(F),
    [Map, Directions] = binary:split(Bin, <<"\n\n">>),
    M = parse_map(Map),
    #state{map=M,
           pos=starting_pos(M, 1, 1),
           directions=parse_dirs(Directions, 0, [])}.

sample() -> input("day22_sample.txt").
input() -> input("day22.txt").

turn(#state{face=F}=S, "R") ->
    S#state{face=case F of
                     3 -> 0;
                     N -> N+1 end};
turn(#state{face=F}=S, "L") ->
    S#state{face=case F of
                     0 -> 3;
                     N -> N-1 end}.

%% Facing to {Dx,Dy}
delta(0) -> {1,0};
delta(1) -> {0,1};
delta(2) -> {-1,0};
delta(3) -> {0,-1}.

wrap({W,H,_}=M, {X,Y}, {Dx, Dy}) ->
    Start = case {Dx, Dy} of
                {1,0} -> {1,Y};
                {-1,0} -> {W,Y};
                {0,1} -> {X,1};
                {0,-1} -> {X,H}
            end,
    find_something(M, Start, {Dx,Dy}).

find_something(M, {X,Y}, {Dx,Dy}) ->
    case at(M, {X,Y}) of
        nothing -> find_something(M, {X+Dx,Y+Dy}, {Dx,Dy});
        Something -> {X, Y, Something}
    end.


next_pos(#state{map=M, pos = {X,Y}, face=F}=S0) ->
    {Dx,Dy} = delta(F),
    NewPos = {X+Dx,Y+Dy},
    case at(M, NewPos) of
        $. -> S0#state{pos = NewPos};  %% pos found
        $# -> S0; %% wall, don't move
        nothing ->
            %% we need to wrap around and check again
            {Wx, Wy, Wat} = wrap(M, {X,Y}, {Dx,Dy}),
            case Wat of
                $. -> S0#state{pos={Wx,Wy}};
                $# -> S0
            end
    end.

process_direction(S0, 0) -> S0;
process_direction(S0, Walk) when is_integer(Walk) ->
    process_direction(next_pos(S0), Walk-1);
process_direction(S0, Turn) -> turn(S0, Turn).

part1() ->
    Start = input(),
    End = lists:foldl(fun(Dir, S0) -> process_direction(S0, Dir) end,
                      Start, Start#state.directions),
    {X,Y} = End#state.pos,
    1000*Y + 4*X + End#state.face.
