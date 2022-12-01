-module(day1).
-compile(export_all).

input() ->
    fileutil:lines("day1.txt").

calories() ->
    [ lists:sum(lists:map(fun list_to_integer/1, Elf)) || Elf <- listutil:splitgroups("", input()) ].

most_calories(Elves) ->
    lists:max(Elves).

part1() ->
    most_calories(calories()).

part2() ->
    [C1,C2,C3 | _] = lists:reverse(lists:sort(calories())),
    C1 + C2 + C3.

day1_binary() ->
    {ok, Input} = file:read_file("day1.txt"),
    [First,Second,Third | _] =
        lists:reverse(lists:sort(
                        lists:map(fun(Cals) ->
                                          lists:foldr(fun(<<>>, Acc) -> Acc;
                                                         (B, Acc) -> Acc + binary_to_integer(B)
                                                      end,
                                                      0, binary:split(Cals,<<"\n">>,[global]))
                                  end,
                                  binary:split(Input, <<"\n\n">>,[global])))),
    {part1, First,
     part2, First + Second + Third}.
