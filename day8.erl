-module(day8).
-compile(export_all).

input() ->
    {ok,Input} = file:read_file("day8.txt"),
    {Width,1} = binary:match(Input, <<"\n">>),
    Height = size(Input) div (Width+1),
    {Width, Height, Input}.

at(X,Y, {W, _, Input}) ->
    binary:at(Input, Y*(W+1)+X).

left(X, Y) -> [{X1,Y} || X1 <- lists:seq(X-1,0,-1)].
right(X, Y, {W,_,_}) -> [{X1,Y} || X1 <- lists:seq(X+1,W-1)].
up(X,Y) -> [{X,Y1} || Y1 <- lists:seq(Y-1,0,-1)].
down(X,Y, {_,H,_}) -> [{X,Y1} || Y1 <- lists:seq(Y+1,H-1)].


visible(X, Y, Forest) ->
    Here = at(X,Y,Forest),
    lists:any(
      fun(Positions) ->
              lists:all(fun({X1,Y1}) -> at(X1,Y1,Forest) < Here end,
                        Positions)
      end,
      [left(X,Y),
       right(X,Y,Forest),
       up(X,Y),
       down(X,Y,Forest)]).

count_visible({W,H,_}=Forest) ->
    length([ 1 || X <- lists:seq(0,W-1),
                  Y <- lists:seq(0,H-1),
                  visible(X,Y,Forest) ]).

part1() -> count_visible(input()).


scenic_count(Acc, Tree, X, Y, DX, DY, {W,H,_}=Forest)
  when X >= 0 andalso X < W andalso
       Y >= 0 andalso Y < H ->
    case at(X,Y,Forest) >= Tree of
        true -> Acc+1;
        false -> scenic_count(Acc+1, Tree, X+DX, Y+DY, DX, DY, Forest)
    end;
scenic_count(Acc, _, _, _, _, _, _) -> Acc.



scenic_score(X, Y, Forest) ->
    Here = at(X, Y, Forest),
    scenic_count(0, Here, X-1, Y, -1, 0, Forest) *
        scenic_count(0, Here, X+1, Y, 1, 0, Forest) *
        scenic_count(0, Here, X, Y-1, 0, -1, Forest) *
        scenic_count(0, Here, X, Y+1, 0, 1, Forest).

max_scenic_score(Max, X, Y, {_,H,_}) when Y == H -> Max;
max_scenic_score(Max, X, Y, {W,_,_}=F) when X == W ->
    max_scenic_score(Max, 0, Y+1, F);
max_scenic_score(Max, X, Y, F) ->
    At = scenic_score(X,Y,F),
    max_scenic_score(max(At,Max), X+1, Y, F).

part2() ->
    max_scenic_score(0, 0, 0, input()).
