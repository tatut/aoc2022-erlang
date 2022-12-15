-module(day15).
-compile(export_all).
-record(sensor, {pos, closest}).

input() -> [parse_sensor(L) || L <- fileutil:lines("day15.txt")].

parse_sensor(Line) ->
    {match, Matches} = re:run(Line, "-?\\d+",[global]),
    [X1,Y1,X2,Y2] = [ list_to_integer(string:substr(Line, Start+1, Len)) || [{Start,Len}] <- Matches ],
    #sensor{pos={X1,Y1}, closest={X2,Y2}}.

dist({X1,Y1}, {X2,Y2}) ->
    abs(X1-X2) + abs(Y1-Y2).

coverage(#sensor{pos={Sx,Sy}, closest={Cx,Cy}}, Y) ->
    Len = dist({Sx,Sy},{Cx,Cy}),
    Xlen = Len - abs(Sy - Y),
    if Xlen < 1 -> none;
       true -> {Sx - Xlen, Sx + Xlen}
    end.

overlaps({_,H1},{L2,_}) when H1 >= L2 -> true;
overlaps(_,_) -> false.

combine({L1,H1},{L2,H2}) -> {min(L1,L2), max(H1,H2)}.

gaps([_], Gaps) -> Gaps;
gaps([Cov1, Cov2 | Rest], Gaps) ->
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
    Row = 2000000,
    Sensors = input(),
    {MinX,MaxX} = lists:foldl(fun xrange/2, {0,0}, Sensors),
    Coverages = lists:dropwhile(fun(none) -> true; (_) -> false end,
                                lists:sort(
                                  [ coverage(S,Row) || S <- Sensors])),
    Gaps = gaps(Coverages, 0),
    {Xstart,_} = hd(Coverages),
    Before = max(0, Xstart - MinX),
    {_,Xend} = lists:last(Coverages),
    After = max(0, MaxX - Xend),
    (MaxX-MinX) - (Before + Gaps + After).

% find beacon  iterate possible positions by row, find first
part2() ->
    {X,Y} = possible_position({0,4000000}, input(), 0),
    Y + 4000000*X.

mind_the_gap(_, [_]) -> none;
mind_the_gap({MinX,MaxX}, [Cov1, Cov2 | Rest]) ->
    case overlaps(Cov1,Cov2) of
        true -> mind_the_gap({MinX,MaxX}, [combine(Cov1,Cov2) | Rest]);
        false ->
            {_,H1} = Cov1, {L2,_} = Cov2,
            GapLen = min(L2,MaxX) - max(MinX,H1) - 1,
            if GapLen > 0 -> H1+1;
               true -> mind_the_gap({MinX,MaxX}, [Cov2 | Rest])
            end
    end.

possible_position({_,MaxX},_,Y) when Y > MaxX -> error;
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

part2_workers(NumWorkers) ->
    Sensors = input(),
    lists:foreach(fun(N) -> spawn(?MODULE, possible_positions_worker,
                                  [self(), {0,4000000}, Sensors, N, NumWorkers]) end,
                  lists:seq(0, NumWorkers-1)),
    receive {X,Y} -> Y + 4000000*X
    after 30000 -> timeout
    end.

possible_positions_worker(_, {_,Max}, _, Y, _) when Y > Max -> none;
possible_positions_worker(ResultPid, {Min,Max}, Sensors, Y, YStep) ->
    Coverages =
        lists:takewhile(fun({Xlo,_}) when Xlo < Max -> true;
                           (_) -> false
                        end,
                        lists:dropwhile(fun(none) -> true;
                                           ({_,Xhi}) when Xhi < Min -> true;
                                           (_) -> false end,
                                        lists:sort(
                                          [ coverage(S,Y) || S <- Sensors]))),
    case mind_the_gap({Min,Max}, Coverages) of
        none -> possible_positions_worker(ResultPid, {Min,Max}, Sensors, Y+YStep, YStep);
        X -> ResultPid ! {X,Y}
    end.
