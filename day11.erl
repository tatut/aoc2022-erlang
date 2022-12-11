-module(day11).
-compile(export_all).

-type arg() :: atom() | integer().
-type op() :: '*' | '+'.
-record(monkey, {id :: integer(),
                 items :: [integer()],
                 op :: {op(), arg(), arg()},
                 test :: integer(), % divisible by
                 if_true :: integer(), % throw to monkey id
                 if_false :: integer(),
                 inspections = 0 :: integer() }).

input() ->
    {ok, Input} = file:read_file("day11.txt"),
    list_to_tuple(
      [parse_monkey(
         lists:map(fun binary_to_list/1, binary:split(M, <<"\n">>, [global]))) ||
          M <- binary:split(Input, <<"\n\n">>, [global])]).

parse_arg("old") -> old;
parse_arg(X) -> list_to_integer(X).


parse_monkey(Lines) ->
    [Id, Items, OpL, Test, True, False] = lists:sublist(Lines, 6),
    [Arg1, Op, Arg2] = string:split(lists:sublist(OpL, 20, 1000), " ", all),
    #monkey{
       id = list_to_integer(lists:sublist(Id, 8, length(Id)-8)),
       items = [list_to_integer(X) ||
                   X <- string:split(lists:sublist(Items, 19, 1000), ", ",all)],
       op = {list_to_atom(Op), parse_arg(Arg1), parse_arg(Arg2)},
       test = list_to_integer(lists:sublist(Test, 22, 1000)),
       if_true = list_to_integer(lists:sublist(True, 30, 1000)),
       if_false = list_to_integer(lists:sublist(False, 31, 1000))
      }.

arg(Old, old) -> Old;
arg(_, X) -> X.

%% Execute operation to determine new worry level
psyop(Old, {'*', Arg1, Arg2}) -> arg(Old,Arg1) * arg(Old,Arg2);
psyop(Old, {'+', Arg1, Arg2}) -> arg(Old,Arg1) + arg(Old,Arg2).

monkey_around(Monkeys) ->
    lists:foldl(fun monkey_around/2, Monkeys, lists:seq(1, size(Monkeys))).

update(Monkeys, Idx, Fun) ->
    setelement(Idx, Monkeys, Fun(element(Idx,Monkeys))).

monkey_around(N, Monkeys0) ->
    %% Get the Nth monkey
    #monkey{items=Items, op=Op,
            test=Test,if_true=IfT, if_false=IfF} = element(N, Monkeys0),
    Monkeys1 =
        lists:foldl(
          fun(Item0,Monkeys) ->
                  Item1 = floor(psyop(Item0, Op)/3),
                  ThrowTo =
                      if (Item1 rem Test) == 0 -> IfT+1;
                         true -> IfF+1
                      end,
                  update(Monkeys, ThrowTo,
                         fun(#monkey{items=It}=M) -> M#monkey{items=It++[Item1]} end)
          end, Monkeys0, Items),
    update(Monkeys1, N,
           fun(#monkey{inspections=Ins}=Mo) ->
                   Mo#monkey{items=[],inspections=Ins+length(Items)} end).

play(0, _, Monkeys) -> Monkeys;
play(Rounds, Fun, Monkeys) -> play(Rounds-1, Fun, Fun(Monkeys)).

monkey_business_level(Monkeys) ->
    [Top1,Top2 | _] =
        lists:sort(fun(Ma,Mb) -> Ma#monkey.inspections > Mb#monkey.inspections end,
                   tuple_to_list(Monkeys)),
    Top1#monkey.inspections * Top2#monkey.inspections.

part1() -> monkey_business_level(play(20,fun monkey_around/1, input())).


%%% part2

monkey_around_unrelieved(Monkeys) ->
    lists:foldl(fun monkey_around_unrelieved/2, Monkeys, lists:seq(1, size(Monkeys))).

multiplier(Monkeys) ->
    lists:foldl(fun(#monkey{test=A},B)-> A * B end, 1, tuple_to_list(Monkeys)).

monkey_around_unrelieved(N, Monkeys0) ->
    %% Get the Nth monkey
    #monkey{items=Items, op=Op,
            test=Test,if_true=IfT, if_false=IfF} = element(N, Monkeys0),
    Multiplier = multiplier(Monkeys0),
    Monkeys1 =
        lists:foldl(
          fun(Item0,Monkeys) ->
                  Item1 = psyop(Item0, Op) rem Multiplier,
                  ThrowTo =
                      if (Item1 rem Test) == 0 -> IfT+1;
                         true -> IfF+1
                      end,
                  update(Monkeys, ThrowTo,
                         fun(#monkey{items=It}=M) -> M#monkey{items=It++[Item1]} end)
          end, Monkeys0, Items),
    update(Monkeys1, N,
           fun(#monkey{inspections=Ins}=Mo) ->
                   Mo#monkey{items=[],inspections=Ins+length(Items)} end).


part2() ->
    monkey_business_level(play(10000, fun monkey_around_unrelieved/1, input())).
