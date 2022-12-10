-module(day6).
-compile(export_all).

input() -> hd(fileutil:lines("day6.txt")).

at_distinct(Count, Lst) ->
    case sets:size(sets:from_list(lists:sublist(Lst, 1, Count))) of
        Count -> true;
        _ -> false
    end.

at_sop(Lst) -> at_distinct(4, Lst).
at_som(Lst) -> at_distinct(14, Lst).

marker_pos(I, MarkerFun, Lst) ->
    case MarkerFun(Lst) of
        true -> I;
        _ -> marker_pos(I+1, MarkerFun, tl(Lst))
    end.

part1() -> marker_pos(4, fun at_sop/1, input()).
part2() -> marker_pos(14, fun at_som/1, input()).
