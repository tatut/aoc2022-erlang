-module(day3).
-compile(export_all).

to_set(Bin) -> to_set(sets:new(), Bin).
to_set(Set, <<>>) -> Set;
to_set(Set, <<Item:1/binary,Rest/binary>>) ->
    to_set(sets:add_element(Item, Set), Rest).

find_duplicate(Bin) ->
    {First,Second} = split_binary(Bin, size(Bin) div 2),
    [Dup] = sets:to_list(
              sets:intersection(to_set(First), to_set(Second))),
    Dup.

priority(<<0:1,1:1,1:1,Lower:5>>) -> Lower;
priority(<<_:3, Upper:5>>) -> Upper + 26.

total_priority(Input) ->
    lists:sum([priority(find_duplicate(Rucksack)) ||
                  Rucksack <- binary:split(Input, <<"\n">>, [global]),
                  Rucksack =/= <<>>
              ]).

part1() ->
    {ok, Input} = file:read_file("day3.txt"),
    total_priority(Input).

badges(Acc, [<<>>]) -> Acc;
badges(Acc, [R1, R2, R3 | Rest]) ->
    [B] = sets:to_list(
            sets:intersection(sets:intersection(to_set(R1), to_set(R2)),
                              to_set(R3))),
    badges(Acc + priority(B), Rest).

badges_priority(Input) ->
    badges(0, binary:split(Input, <<"\n">>, [global])).

part2() ->
    {ok, Input} = file:read_file("day3.txt"),
    badges_priority(Input).
