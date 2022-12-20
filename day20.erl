-module(day20).
-compile(export_all).

sample() ->
    initial_state([1, 2, -3, 3, -2, 0, 4]).

input() ->
    lists:enumerate([list_to_integer(L) || L <- fileutil:lines("day20.txt") ]).

initial_state(List) ->
    lists:enumerate(List).

%% Find current index based in initial index
idx(InitialIdx, C) ->
    idx(InitialIdx, C, 1).
idx(InitialIdx0, [{InitialIdx1,Val}|_], Idx) when InitialIdx0 == InitialIdx1 -> {Idx, Val};
idx(InitialIdx, [_|Rest], Idx) -> idx(InitialIdx, Rest, Idx+1).

newidx(Idx0, _, _, 0) -> Idx0;
newidx(Idx0, Len, Dir, HowMany) ->
    Idx1 = Idx0 + Dir,
    newidx(case Idx1 of
               0 -> Len-1;
               1 -> Len;
               Len -> 1;
               _ -> Idx1
           end, Len, Dir, HowMany-1).

removeidx(List, Idx) ->
    [ X || {_, X} <-
               lists:keydelete(Idx, 1, lists:enumerate(List))].

add_to_idx(List, Idx, Item) ->
    lists:sublist(List, 1, Idx-1) ++ [Item] ++ lists:sublist(List, Idx, length(List)).

move(_, _, 0, List) -> List;
move(IdxFn, Idx0, HowMany, List) ->
    Len = length(List),
    %%HowMany = HowMany0 rem length(List),
    Idx1 = IdxFn(Idx0, Len, HowMany),
    if Idx0 == Idx1 -> List;
       true -> Item = lists:nth(Idx0,List),
               %%io:format("~p at ~p => ~p, howmany ~p, len ~p~n",[Item, Idx0, Idx1, HowMany, Len]),
               add_to_idx(removeidx(List, Idx0), Idx1, Item)
    end.

move_pos(IdxFn, InitialIdx, List0) ->
    {Idx0, Val} = idx(InitialIdx, List0),
    %%io:format("~p move, val: ~p , idx: ~p ~n", [InitialIdx, Val, Idx0]),
    move(IdxFn,Idx0, Val, List0).

vals(L) ->
    [element(2, I) || I <- L].


findidx(Pred, List) -> findidx(Pred, List, 1).
findidx(Pred, [I|Items], Idx) ->
    case Pred(I) of
        true -> Idx;
        false -> findidx(Pred, Items, Idx+1)
    end.

mix(IdxFn, S0) ->
    S1 = lists:foldl(fun(Idx,C0) ->
                        C1 = move_pos(IdxFn, Idx, C0),
                        %%io:format("  ~p  =>  ~p ~n", [vals(C0), vals(C1)]),
                        C1
                end,
                     S0, lists:seq(1, length(S0))),
    io:format(" Mixed: ~p ~n", [S1]),
    S1.

sum3(S1) ->
    ZeroPos = findidx(fun({_,0}) -> true; (_) -> false end, S1),
    Len = length(S1),
    Items =
      [ X || {_,X} <- [ lists:nth((ZeroPos+N) rem Len, S1) || N <- [1000, 2000, 3000] ] ],
    io:format("three items: ~p~n", [Items]),
    lists:sum(Items).

part1idx(Idx0, Len, HowMany) ->
    newidx(Idx0, Len, case HowMany of
                          Neg when Neg < 0 -> -1;
                          Pos when Pos > 0 -> 1

                      end,
           abs(HowMany)).

part1() ->
    sum3(mix(fun part1idx/3, input())).

%% 872 is right

part1sample()->
    sum3(mix(fun part1idx/3, sample())).

%%% Part2 multiply by encryption key
-define(ENCRYPTION, 811589153).

part2idx(Idx0, Len, HowMany) ->
    part1idx(Idx0,Len,HowMany).
    %% case HowMany rem Len of
    %%     0 -> Idx0;
    %%     N -> part1idx(Idx0, Len, N)
    %% end.

mixtimes(_, List, Times) when Times == 0 -> List;
mixtimes(IdxFn, List, Times) -> mixtimes(IdxFn, mix(IdxFn, List), Times-1).

part2sample() ->
    S0 = [ {Idx, Num * ?ENCRYPTION} || {Idx,Num} <- sample() ],
    io:format("INITIAL: ~p~n", [S0]),
    sum3(mixtimes(fun part2idx/3, S0, 10)).
