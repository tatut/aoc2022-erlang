-module(day20).
-compile(export_all).

%% model ring as an ETS table that as {InitialIndex, Value, PreIdx, NextIdx}
%% any movement, you just update prev and next to point to each other,
%% then rotate the ring

as_table(Encryption,List) ->
    T = ets:new(day20, [set]),
    Len = length(List),
    Wrap = fun(X) when X == 0 -> Len;
              (X) when X == Len+1 -> 1;
              (X) -> X end,
    [ ets:insert(T, {Idx, Encryption*Val, Wrap(Idx-1), Wrap(Idx+1)}) || {Idx,Val} <- lists:enumerate(List)],
    {ok, T, Len}.

sample(Encryption) -> as_table(Encryption, [1, 2, -3, 3, -2, 0, 4]).
input(Encryption) -> as_table(Encryption, [list_to_integer(L) || L <- fileutil:lines("day20.txt") ]).

at(T, Idx) ->
    [Obj] = ets:lookup(T, Idx),
    Obj.
prev(T, Idx, NewPrev) ->
    ets:update_element(T, Idx, [{3, NewPrev}]).
next(T, Idx, NewNext) ->
    ets:update_element(T, Idx, [{4, NewNext}]).
both(T, Idx, NewPrev, NewNext) ->
    ets:update_element(T, Idx, [{3, NewPrev}, {4, NewNext}]).

%% Move item (by initial index)
move(T, Len, Idx) ->
    {_, Value, Prev, Next} = at(T, Idx),
    case Value of
        0 -> ok;
        V ->
            %% Remove myself by setting prev<->next pointing to each other
            next(T, Prev, Next),
            prev(T, Next, Prev),

            %% Rotate the ring in the wanted direction
            InsertAfter = case V of
                              Pos when Pos > 0 -> rotate(T, Next, 4, Value-1);
                              Neg when Neg < 0 -> rotate(T, Prev, 3, abs(Value))
                          end,
            %%io:format("Move idx ~p val ~p TO AFTER ~p~n", [Idx, Value, at(T,InsertAfter)]),
            {_, _, _, NewNext} = at(T, InsertAfter),
            next(T, InsertAfter, Idx),
            both(T, Idx, InsertAfter, NewNext),
            prev(T, NewNext, Idx),
            ok
    end.

rotate(_, At, _, 0) -> At;
rotate(T, At, Link, Amount) ->
    Next = element(Link, at(T, At)),
    %%io:format("  move ~p => ~p (dir ~p)~n", [At, Next, Link]),
    rotate(T, Next, Link, Amount-1).


vals(_, _, 0, Acc) -> lists:reverse(Acc);
vals(T, At, C, Acc) ->
    {_, V, _, Next} = at(T, At),
    vals(T, Next, C-1, [V|Acc]).

mix(T, Len) ->
    lists:foreach(fun(Idx) ->
                          move(T, Len, Idx)
                          %%io:format("V: ~p~n",[vals(T, Idx, Len, [])])
                  end,
                  lists:seq(1, Len)).

sum3(T) ->
    [[ZeroIdx]] = ets:match(T, {'$1', 0, '_', '_'}),
    Items =
        [ element(2,at(T, Idx)) || Idx <- [ rotate(T, ZeroIdx, 4, N) || N <- [1000, 2000, 3000] ]],
     io:format("three items: ~p~n", [Items]),
    lists:sum(Items).

part1() ->
    {ok, T, Len} = input(1),
    mix(T,Len),
    sum3(T).

%% 872 is right

% Part2 multiply by encryption key
-define(ENCRYPTION, 811589153).

mixtimes(T, Len, 0) -> ok;
mixtimes(T, Len, Times) ->
    mix(T,Len),
    mixtimes(T, Len, Times-1).

part2() ->
    {ok, T, Len} = input(?ENCRYPTION),
    mix(T,Len).

part2sample() ->
    {ok, T, Len} = sample(?ENCRYPTION),
    mixtimes(T,Len,10),
    ets:match(T, '$1').
