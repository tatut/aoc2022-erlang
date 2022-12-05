-module(day5).
-compile(export_all).

input(Filename) ->
    {ok, Input} = file:read_file(Filename),
    [Stacks, Moves] = binary:split(Input, <<"\n\n">>),
    {parse_stacks(binary:split(Stacks,<<"\n">>,[global])),
     lists:flatmap(fun parse_move/1, binary:split(Moves,<<"\n">>,[global]))}.


parse_stack(Idx, Lines) ->
    lists:dropwhile(fun(X) -> X =:= nothing end,
                    lists:map(fun(Line) when size(Line) > Idx ->
                                      case binary:at(Line, Idx) of
                                          $ -> nothing;
                                          Item -> Item
                                      end;
                                 (_) -> nothing
                              end, Lines)).


parse_stacks(Lines) ->
    Lines1 = lists:sublist(Lines, 1, length(Lines)-1), % remove the stack idx line
    list_to_tuple([parse_stack(I, Lines1) || I <- lists:seq(1, 50, 4)]).


parse_move(<<>>) -> [];
parse_move(Bin) ->
    ["move", Count, "from", From, "to", To] =
        string:split(binary_to_list(Bin), " ", all),
    FromIdx = list_to_integer(From),
    ToIdx = list_to_integer(To),
    [{FromIdx, ToIdx} || _ <- lists:seq(1, list_to_integer(Count))].

cratemover9000({From, To}, Stacks) ->
    [Taken | Rest] = element(From, Stacks),
    ToStack = element(To, Stacks),
    setelement(To, setelement(From, Stacks, Rest), [Taken|ToStack]).

top(Stacks) -> [hd(S) || S <- tuple_to_list(Stacks),
                         length(S) > 0].

part1() ->
    {Stacks, Moves} = input("day5.txt"),
    top(lists:foldl(fun cratemover9000/2, Stacks, Moves)).

%% part2 - move all at once, unfortunately I unpacked the moves in parsing part1, need to reimplement

cratemover9001(nothing, Stacks) -> Stacks;
cratemover9001({From,To,Count}, Stacks) ->
    {Taken, RestItems} = lists:split(Count, element(From, Stacks)),
    ToStack = element(To, Stacks),
    setelement(To, setelement(From, Stacks, RestItems), Taken ++ ToStack).

parse_move2(<<>>) -> nothing;
parse_move2(Bin) ->
    ["move", Count, "from", From, "to", To] =
        string:split(binary_to_list(Bin), " ", all),
    {list_to_integer(From),
     list_to_integer(To),
     list_to_integer(Count)}.

input2(Filename) ->
    {ok, Input} = file:read_file(Filename),
    [Stacks, Moves] = binary:split(Input, <<"\n\n">>),
    {parse_stacks(binary:split(Stacks,<<"\n">>,[global])),
     lists:map(fun parse_move2/1, binary:split(Moves,<<"\n">>,[global]))}.

part2() ->
    {Stacks, Moves} = input2("day5.txt"),
    top(lists:foldl(fun cratemover9001/2, Stacks, Moves)).
