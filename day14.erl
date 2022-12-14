-module(day14).
-compile(export_all).

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

fill_rock(T, Pos1, Pos2) when Pos1 == Pos2 -> ets:insert(T, {Pos1,$#});
fill_rock(T, {X1,Y1}=Pos1, {X2,Y2}=Pos2) ->
    %% Move toward pos2
    NewPos = {X1 + sign(X2-X1), Y1 + sign(Y2-Y1)},
    ets:insert(T, [{Pos1,$#}, {NewPos,$#}]),
    fill_rock(T, NewPos, Pos2).

input() ->
    T = ets:new(day14, [set]), % ordered_set ?
    lists:foreach(fun(L) ->
                          [P1 | Ps] = [ parse_point(P) || P <- string:split(L, " -> ", all)],
                          lists:foldl(fun(Point,Prev) ->
                                              fill_rock(T, Prev, Point),
                                              Point
                                      end, P1, Ps)
                  end, fileutil:lines("day14.txt")),
    T.

parse_point(Str) ->
    [X,Y] = string:split(Str, ","),
    {list_to_integer(X), list_to_integer(Y)}.

draw() ->
    draw(input(), "day14.xpm").

draw(T, File) ->
    xpm:draw(File, [{$#, {0,0,0}},
                    {$., {255,255,255}},
                    {$%, {194,178,128}}],
             T, $.).

at(T, Pos) ->
    case ets:lookup(T, Pos) of
        [{_, Ch}] -> Ch;
        _ -> $.
    end.

at(T, Pos, Ch) ->
    ets:insert(T, {Pos, Ch}).

can_move(T, Pos) ->
    case at(T, Pos) of
        $. -> true;
        _ -> false
    end.

drop_sand(_, H, {_, Y}) when Y > H -> abyss;
drop_sand(T, H, {X,Y}) ->
    case can_move(T, {X,Y+1}) of
        true -> drop_sand(T, H, {X,Y+1});
        false -> case can_move(T, {X-1,Y+1}) of
                     true -> drop_sand(T, H, {X-1,Y+1});
                     false -> case can_move(T, {X+1,Y+1}) of
                                  true -> drop_sand(T, H, {X+1,Y+1});
                                  false ->
                                      %% Can't move at all, stay here then
                                      at(T, {X,Y}, $%)
                              end
                 end
    end.

sandstorm(T, H, DropSand, I) ->

    %% Not necessary for the solution, but fun to create images every thousandth steps
    %% to make an animated gif
    if I rem 100 == 0 ->
            draw(T, "day14_" ++ io_lib:format("~6.10.0b", [I]) ++ ".xpm");
       true -> ok
    end,

    case DropSand(T, H, {500,0}) of
        abyss -> I;
        _ -> sandstorm(T, H, DropSand, I+1)
    end.

cave_size(T) ->
    ets:foldl(fun({{X,Y},_},{Mx,My}) ->
                      {max(Mx,X), max(My,Y)}
              end,
              {0,0},
              T).

solve(DropSand) ->
    T = input(),
    {_,MaxY} = cave_size(T),
    sandstorm(T, MaxY, DropSand, 0).

part1() -> solve(fun drop_sand/3).

%%%%% part2  inifinite floor at maxy+2

part2() ->
    1 + solve(fun drop_sand_infinite/3).

drop_sand_infinite(T, H, {X,Y}) when Y == H+1 ->
    at(T, {X,Y}, $%);
drop_sand_infinite(T, H, {X,Y}) ->
    case can_move(T, {X,Y+1}) of
        true -> drop_sand_infinite(T, H, {X,Y+1});
        false -> case can_move(T, {X-1,Y+1}) of
                     true -> drop_sand_infinite(T, H, {X-1,Y+1});
                     false -> case can_move(T, {X+1,Y+1}) of
                                  true -> drop_sand_infinite(T, H, {X+1,Y+1});
                                  false ->
                                      %% Can't move at all, stay here then
                                      at(T, {X,Y}, $%),
                                      if {X,Y} == {500,0} -> abyss;
                                         true -> ok
                                      end
                              end
                 end
    end.

%% Generate images
%% $ for i in day14_*.xpm; do convert -extract 250x250+300+00 $i $i.extract.xpm ; done
%% $ convert  *.extract.xpm day14_part2_small.gif
