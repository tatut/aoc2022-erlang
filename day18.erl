-module(day18).
-compile(export_all).

input() ->
    sets:from_list(
      [ {list_to_integer(X),
         list_to_integer(Y),
         list_to_integer(Z)}
        || {X,Y,Z} <- fileutil:lines("day18.txt",
                                     "(\\d+),(\\d+),(\\d+)") ]).

covered_sides(Points, {X,Y,Z}) ->
    C = fun(Dx,Dy,Dz) ->
                case sets:is_element({X+Dx,Y+Dy,Z+Dz}, Points) of
                    true -> 1;
                    false -> 0
                end
        end,
    C(1,0,0) + C(-1,0,0) +
        C(0,1,0) + C(0,-1,0) +
        C(0,0,1) + C(0,0,-1).

all_covered_sides(Points) ->
    lists:sum(
      [covered_sides(Points, P) || P <- sets:to_list(Points)]).

part1() ->
    Points = input(),
    Len = length(sets:to_list(Points)),
    Len * 6 - all_covered_sides(Points).


%% Part2 steam expands to neighbors, count encountered points

range(Elt, Points) ->
    sets:fold(fun(P, {Min,Max}) ->
                      V = element(Elt, P),
                      {case Min of
                           undefined -> V;
                           _ -> min(V,Min)
                       end,
                       case Max of
                           undefined -> V;
                           _ -> max(V,Max)
                       end}
              end, {undefined,undefined}, Points).

neighbors({X,Y,Z}) ->
    [{X+1,Y,Z},{X-1,Y,Z},
     {X,Y+1,Z},{X,Y-1,Z},
     {X,Y,Z+1},{X,Y,Z-1}].


fill({X,Y,Z}, {{Xmin,Xmax}, {Ymin,Ymax}, {Zmin,Zmax}}, _, Visited, Count) when
      X < Xmin - 1 orelse X > Xmax + 1 orelse
      Y < Ymin - 1 orelse Y > Ymax + 1 orelse
      Z < Zmin - 1 orelse Z > Zmax + 1 -> {Visited, Count};
fill(At, Ranges, Points, Visited0, Count0) ->
    lists:foldl(
      fun(N,{Visited,Count}) ->
              case sets:is_element(N, Visited) of
                  true -> {Visited,Count};
                  false ->
                      %% Not visited yet, see if we encounter lava
                      case sets:is_element(N, Points) of
                          true -> {Visited, Count+1};
                          false ->
                              VisOut = sets:add_element(N, Visited),
                              fill(N, Ranges, Points, VisOut, Count)
                      end
              end
      end, {Visited0, Count0}, neighbors(At)).

part2() ->
    Points = input(),
    Xr = range(1, Points),
    Yr = range(2, Points),
    Zr = range(3, Points),
    Start = {element(1,Xr)-1, element(1,Yr)-1, element(1,Zr)-1},
    {_, Count} = fill(Start,
                      {Xr, Yr, Zr},
                      Points,
                      sets:new(),
                      0),
    Count.
