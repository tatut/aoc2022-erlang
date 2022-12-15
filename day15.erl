-module(day15).
-compile(export_all).
-record(sensor, {pos, closest}).

input(File) ->
    [parse_sensor(L) || L <- fileutil:lines(File)].

parse_sensor(Line) ->
    {match, Matches} = re:run(Line, "-?\\d+",[global]),
    [X1,Y1,X2,Y2] = [ list_to_integer(string:substr(Line, Start+1, Len)) || [{Start,Len}] <- Matches ],
    #sensor{pos={X1,Y1}, closest={X2,Y2}}.

dist({X1,Y1}, {X2,Y2}) ->
    abs(X1-X2) + abs(Y1-Y2).

coverage(#sensor{pos={Sx,Sy}, closest={Cx,Cy}}=S, Y) ->
    Len = dist({Sx,Sy},{Cx,Cy}),
    Xlen = Len - abs(Sy - Y),
    if Xlen < 1 ->
            %%io:format(" cov len, ~p, len: ~p , xlen:  ~p~n", [S,Len,Xlen]),
            none;
       true -> {Sx - Xlen, Sx + Xlen}
    end.

overlaps({L1,H1},{L2,H2}) when H1 >= L2 -> true;
overlaps(_,_) -> false.

combine({L1,H1},{L2,H2}) -> {min(L1,L2), max(H1,H2)}.

gaps([_], Gaps) -> Gaps;
gaps([Cov1, Cov2 | Rest], Gaps) ->
    %%io:format("S1 ~p  S2 ~p~n", [Cov1, Cov2]),
    case overlaps(Cov1,Cov2) of
        true -> gaps([combine(Cov1,Cov2) | Rest], Gaps);
        false ->
            {_,H1} = Cov1, {L2,_} = Cov2,
            gaps([Cov2 | Rest], Gaps + (L2-H1))
    end.

minp({X1,Y1},{X2,Y2}) -> {min(X1,X2),min(Y1,Y2)}.
maxp({X1,Y1},{X2,Y2}) -> {max(X1,X2),max(Y1,Y2)}.

xrange(#sensor{pos=P, closest = C}, {MinX,MaxX}) ->
    Len = dist(P,C),
    {Px1,_} = minp(P,C),
    {Px2,_} = maxp(P,C),
    {min(Px1-Len,MinX), max(Px2+Len,MaxX)}.


part1() ->
    %%File = "day15_sample.txt", Row = 10,
    File = "day15.txt", Row = 2000000,
    Sensors = input(File),
    {MinX,MaxX} = lists:foldl(fun xrange/2, {0,0}, Sensors),
    io:format("xrange: ~p~n", [{MinX,MaxX}]),
    Coverages = lists:dropwhile(fun(none) -> true; (_) -> false end,
                                lists:sort(
                                  [ coverage(S,Row) || S <- Sensors])),
    Gaps = gaps(Coverages, 0),
    {Xstart,_} = hd(Coverages),
    Before = if Xstart > MinX -> Xstart - MinX;
                true -> 0
             end,
    {_,Xend} = lists:last(Coverages),
    After = if Xend < MaxX -> MaxX - Xend;
               true -> 0
            end,
    (MaxX-MinX) - (Before + Gaps + After).

%% part1 => 4424278

% find beacon  iterate possible positions by row, find first
part2() ->
    %%File = "day15_sample.txt", MaxCoord = 20,
    File = "day15.txt",
    Sensors = input(File),
    XRange = {0,4000000}, %%lists:foldl(fun xrange/2, {0,0}, Sensors),
    io:format("xrange: ~p~n", [XRange]),
    GapPos = possible_position(XRange, Sensors, 0),
    io:format("Gap pos: ~p ~n", [GapPos]),
    {X,Y} = GapPos,
    Y + 4000000*X.

mind_the_gap(_, [_]) -> none;
mind_the_gap({MinX,MaxX}, [Cov1, Cov2 | Rest]) ->
    %%io:format("S1 ~p  S2 ~p~n", [Cov1, Cov2]),
    case overlaps(Cov1,Cov2) of
        true -> mind_the_gap({MinX,MaxX}, [combine(Cov1,Cov2) | Rest]);
        false ->
            {_,H1} = Cov1, {L2,_} = Cov2,
            GapLen = min(L2,MaxX) - max(MinX,H1) - 1,
            if GapLen > 0 -> H1+1;
               true -> mind_the_gap({MinX,MaxX}, [Cov2 | Rest])
            end
    end.

possible_position(_,_,Y) when Y > 4000000 -> error;
possible_position({MinX,MaxX}, Sensors, Y) ->
    Coverages =
        lists:takewhile(fun({Xlo,_}) when Xlo < MaxX -> true;
                           (_) -> false
                        end,
                        lists:dropwhile(fun(none) -> true;
                                           ({_,Xhi}) when Xhi < MinX -> true;
                                           (_) -> false end,
                                        lists:sort(
                                          [ coverage(S,Y) || S <- Sensors]))),
    case mind_the_gap({MinX,MaxX}, Coverages) of
        none -> possible_position({MinX,MaxX}, Sensors, Y+1);
        X -> {X,Y}
    end.
