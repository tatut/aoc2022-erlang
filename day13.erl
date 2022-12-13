-module(day13).
-compile(export_all).

eval(Line)->
    {ok,Tokens,_} = erl_scan:string(Line++"."),
    {ok,Form} = erl_parse:parse_term(Tokens),
    Form.

pairs([]) -> [];
pairs([Left,Right|Rest]) ->
    [{Left,Right} | pairs(Rest)].

input() ->
    pairs([eval(Line) || Line <- fileutil:lines("day13.txt"),
                         Line =/= ""]).

right_order(L,R) when is_integer(L) and is_integer(R) ->
    if L < R -> true;
       L > R -> false;
       L == R -> same
    end;
right_order(Ls, Rs) when is_list(Ls) andalso is_list(Rs) ->
    Ll = length(Ls),
    Rl = length(Rs),
    if Ll == 0 andalso Rl == 0 -> same;
       Ll == 0 andalso Rl > 0 -> true;
       Ll > 0 andalso Rl == 0 -> false;
       true -> case right_order(hd(Ls), hd(Rs)) of
                   same -> right_order(tl(Ls), tl(Rs));
                   X -> X
               end
    end;
right_order(L, Rs) when is_integer(L) ->
    right_order([L], Rs);
right_order(Ls, R) when is_integer(R) ->
    right_order(Ls, [R]).

right_order({L,R}) -> right_order(L,R).

part1() ->
    Pairs = input(),
    lists:foldl(fun(I, Sum) ->
                        case right_order(lists:nth(I, Pairs)) of
                            true -> Sum + I;
                            false -> Sum
                        end
                end,
                0,
                lists:seq(1, length(Pairs))).


%% part2 sort all lines

part2() ->
    Packets = [ [[2]], [[6]] | [eval(Line) || Line <- fileutil:lines("day13.txt"),
                                              Line =/= ""]],
    Sorted = lists:enumerate(lists:sort(fun right_order/2, Packets)),
    {D1, _} = lists:keyfind([[2]], 2, Sorted),
    {D2, _} = lists:keyfind([[6]], 2, Sorted),
    D1 * D2.
