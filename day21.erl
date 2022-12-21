-module(day21).
-compile(export_all).

number_or_op(Op) ->
    try
        list_to_integer(Op)
    catch
        error:badarg ->
            fileutil:matches(Op, "^(\\w+) (.) (\\w+)$")

    end.

input(File) ->
    lists:foldl(fun({Monkey,NumberOrOp}, M) ->
                        maps:put(Monkey, number_or_op(NumberOrOp), M) end,
                #{},
                fileutil:lines(File, "^(\\w+): (.*)$")).

input() -> input("day21.txt").
sample() -> input("day21_sample.txt").

eval(Monkey, Ctx) ->
    Val = maps:get(Monkey, Ctx),
    case Val of
        N when is_integer(N) -> Ctx;
        {Arg1, Op, Arg2} ->
            Ctx1 = lists:foldl(fun eval/2, Ctx, [Arg1, Arg2]),
            Res = op(Op, maps:get(Arg1, Ctx1), maps:get(Arg2, Ctx1)),
            maps:put(Monkey, Res, Ctx1)
    end.

eval1(Monkey,Ctx) ->
    maps:get(Monkey, eval(Monkey,Ctx)).

op("*", Arg1, Arg2) -> Arg1 * Arg2;
op("-", Arg1, Arg2) -> Arg1 - Arg2;
op("/", Arg1, Arg2) -> Arg1 div Arg2;
op("+", Arg1, Arg2) -> Arg1 + Arg2;
op("=", Arg1, Arg2) ->
    if Arg1 == Arg2 -> ok;
       true -> error
    end.

part1() ->
    maps:get("root", eval("root", input())).


%% Part2 determine "humn" so that "root" op (now equality check) will match

%% Naive approach, just try all the values
%% find_humn(Input, Val) ->
%%     case (Val rem 10000) == 0 of
%%         true -> io:format("Val ~p~n", [Val]);
%%         false -> ok
%%     end,
%%     M = maps:put("humn", Val, Input),
%%     Ctx = eval("root", M),
%%     case maps:get("root", Ctx) of
%%         ok -> Val;
%%         error -> find_humn(Input, Val + 1)
%%     end.

%% works for sample
%% part2naive() ->
%%     Input = maps:update_with("root", fun({Arg1,_,Arg2}) -> {Arg1,"=", Arg2} end, sample()),
%%     find_humn(Input, 0).


%% Find something that refers to given monkey name as argument
find_name(Name, Iter0) ->
    case maps:next(Iter0) of
        none -> unknown;
        {K, {Name, _, _}=V, _} -> {K, V};
        {K, {_, _, Name}=V, _} -> {K, V};
        {Name, Num, _} when is_integer(Num) -> {Name, Num};
        {_, _, Iter1}  -> find_name(Name, Iter1)
    end.

find(Name, M) -> find_name(Name, maps:iterator(M)).


solve_chain(Acc, []) -> Acc;
solve_chain(Acc, [{Arg1,Op,Arg2}|Ops]) ->
    %% One of the arguments is a number
    N = if is_number(Arg1) -> Arg1;
           true -> Arg2
        end,
    Acc1 = case Op of
               "=" -> N;
               "*" -> Acc div N;
               "/" -> Acc * N;
               "+" -> Acc - N;
               "-" -> if is_number(Arg1) -> N - Acc;
                         true -> Acc + N
                      end
           end,
    solve_chain(Acc1, Ops).

find_humn(_, "root", Acc) -> solve_chain(0, Acc);
find_humn(M, Current, Acc) ->
    {Next, {Arg1, Op, Arg2}} = find(Current, M),
    find_humn(M, Next, [case Current of
                            Arg1 -> {Arg1, Op, eval1(Arg2, M)};
                            Arg2 -> {eval1(Arg1, M), Op, Arg2}
                        end | Acc]).

part2() ->
    M = maps:put("humn", humn, maps:update_with("root", fun({Arg1,_,Arg2}) -> {Arg1,"=", Arg2} end,
                                                input())),
    find_humn(M, "humn", []).
