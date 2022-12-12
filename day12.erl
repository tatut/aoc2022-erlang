-module(day12).
-compile(export_all).

input() ->
    {ok, Input} = file:read_file("day12.txt"),
    {Width,1} = binary:match(Input, <<"\n">>),
    Height = size(Input) div (Width+1),
    {Width, Height, Input}.

at({W,_,Input}, {X,Y}) -> binary:at(Input,Y*(W+1)+X).

e($S) -> $a;
e($E) -> $z;
e(X) -> X.

find_pos(Map, Marker) -> find_pos(Map,Marker, {0,0}).
find_pos({W,_,_}=Map, Marker, {X, Y}) when X >= W -> find_pos(Map, Marker, {0, Y+1});
find_pos(Map, Marker, {X, Y}) ->
    case at(Map, {X,Y}) of
        Marker -> {X,Y};
        _ -> find_pos(Map,Marker,{X+1,Y})
    end.

neighbors({W,H,_}, {X, Y}) ->
    [{X1,Y1} || {X1,Y1} <- [{X-1,Y}, {X+1,Y}, {X, Y-1}, {X, Y+1}],
                (X1 >= 0) and (X1 < W) and (Y1 >= 0) and (Y1 < H)].

possible_steps(Map, From) ->
    MaxElevation = e(at(Map, From)) + 1,
    [To || To <- neighbors(Map, From),
           e(at(Map, To)) =< MaxElevation].

score({X1,Y1},{X2,Y2}) -> abs(X1-X2)+abs(Y1-Y2).

build_graph(Map) ->
    G = digraph:new(),
    build_graph(G, Map, {0, 0}),
    G.

build_graph(_, {_,H,_}, {_, Y}) when Y >= H -> ok;
build_graph(G, {W,_,_}=Map, {X, Y}) when X >= W -> build_graph(G,Map,{0,Y+1});
build_graph(G, Map, {X,Y}) ->
    From = digraph:add_vertex(G, {X,Y}, at(Map, {X,Y})),
    lists:foreach(fun(N) ->
                          To = digraph:add_vertex(G, N, at(Map, N)),
                          digraph:add_edge(G, From, To)
                  end,
                  possible_steps(Map, {X,Y})),
    build_graph(G, Map, {X+1,Y}).

part1() ->
    Map = input(),
    Start = find_pos(Map, $S),
    End = find_pos(Map, $E),
    G = build_graph(Map),
    length(digraph:get_short_path(G, Start, End)) - 1.

part2() ->
    Map = input(),
    G = build_graph(Map),
    End = find_pos(Map, $E),
    lists:foldl(fun(V, Acc) ->
                        {V, Elev} = digraph:vertex(G, V),
                        if Elev == $a ->
                                case digraph:get_short_path(G, V, End) of
                                    false -> Acc;
                                    Path -> min(Acc, length(Path)-1)
                                end;
                           true -> Acc
                        end
                end,
                999999999999,
                digraph:vertices(G)).


xpm_col(Ch) ->
    N = 100 + ((Ch - $a) * 6),
    Hex = io_lib:format("~2.16.0b",[N]),
    ["\"", Ch, " c #", Hex,Hex,Hex, "\",\n"].
xpm_head() -> <<"/* XPM */\nstatic char * XFACE[] = {\n">>.
xpm_info({W,H,_}) ->
    ["\"", integer_to_list(W), " ", integer_to_list(H), " 27 1\",\n",
     [ xpm_col(C) || C <- lists:seq($a,$z)],
    %% add color for path
     "\"@ c #ff0000\",\n"
    ].
xpm_line({W,H,_}=Map, Y, Path) ->
    ["\"",
     [ case sets:is_element({X,Y}, Path) of
           true -> "@";
           false -> at(Map, {X,Y})
       end || X <- lists:seq(0,W-1) ],
     if Y == H-1 -> "\"\n";
        true -> "\",\n"
     end].

xpm_foot() -> "};".


draw() ->
    Map = {_,H,_} = input(),
    Start = find_pos(Map, $S),
    End = find_pos(Map, $E),
    G = build_graph(Map),
    Path = sets:from_list(digraph:get_short_path(G, Start, End)),
    file:write_file("day12.xpm",
                    [xpm_head(),
                     xpm_info(Map),
                     [ xpm_line(Map, Line, Path) || Line <- lists:seq(0, H-1) ],
                     xpm_foot()]).
