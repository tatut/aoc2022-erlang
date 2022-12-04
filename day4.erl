-module(day4).
-compile(export_all).

parse_range(Str) -> lists:map(fun list_to_integer/1, string:split(Str, "-")).
parse_pair(Str) -> lists:map(fun parse_range/1, string:split(Str, ",")).

parse_elves(Lines) ->
    lists:map(fun parse_pair/1, Lines).

contains([U1,L1], [U2,L2]) when U1 =< U2 andalso L1 >= L2 -> true;
contains([U1,L1], [U2,L2]) when U2 =< U1 andalso L2 >= L1 -> true;
contains(_,_) -> false.

how_many(Acc, _, []) -> Acc;
how_many(Acc, Fun, [[E1,E2] | Elves]) ->
    C = case Fun(E1,E2) of
            true -> 1;
            false -> 0
        end,
    how_many(Acc + C, Fun, Elves).

input() -> parse_elves(fileutil:lines("day4.txt")).

part1() -> how_many(0, fun contains/2, input()).

overlaps([U1,L1], [U2,_]) when U1 < U1 andalso L1 < U2 -> false;
overlaps([U1,L1], [_,L2]) when U1 > L2 andalso L1 > L2 -> false;
overlaps([U1,_], [U2,L2]) when U2 < U1 andalso L2 < U1 -> false;
overlaps([_,L1], [U2,L2]) when U2 > L1 andalso L2 > L1 -> false;
overlaps(_,_) -> true.

part2() -> how_many(0, fun overlaps/2, input()).
