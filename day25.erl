-module(day25).
-compile(export_all).

input(File) ->
     fileutil:lines(File).

input() -> input("day25.txt").
sample() -> input("day25_sample.txt").

snafu($2) -> 2;
snafu($1) -> 1;
snafu($0) -> 0;
snafu($-) -> -1;
snafu($=) -> -2.

digit(2) -> $2;
digit(1) -> $1;
digit(0) -> $0;
digit(-1) -> $-;
digit(-2) -> $=.

snafu_to_integer(List) ->
    snafu_to_integer(lists:reverse(List), 1, 0).
snafu_to_integer([D|Rest], Pos, Acc) ->
    snafu_to_integer(Rest, Pos*5, Acc+(Pos*snafu(D)));
snafu_to_integer([], _, Acc) -> Acc.

largest_pos(Int, Pos, Acc) ->
    if Int > (Acc+2*Pos) ->
            largest_pos(Int, Pos*5, Acc+(2*Pos));
       true -> Pos
    end.

largest_pos(Int) -> largest_pos(Int, 1, 0).

largest_num(1) -> 2;
largest_num(Pos) ->
    (2 * Pos) + largest_num(Pos div 5).

%%   Decimal          SNAFU
%%         1              1
%%         2              2
%%         3             1=
%%         4             1-
%%         5             10
%%         6             11
%%         7             12
%%         8             2=
%%         9             2-
%%        10             20
%%        15            1=0
%%        20            1-0
%%      2022         1=11-2
%%     12345        1-0---0
%% 314159265  1121-1110-1=0

fuel_sum(Input) ->
    lists:sum([snafu_to_integer(F) || F <- Input]).

test_num(Int,Snafu) ->
    io:format("Dec: ~p, Snafu: ~p~n", [Int,Snafu]),
    Int = snafu_to_integer(Snafu),
    io:format("  snafu->int ok~n",[]),
    Snafu = int_to_snafu(Int),
    io:format("  int->snafu ok~n", [])
    .

test() ->
    [ test_num(Int,Snafu) ||
        [Int,Snafu] <-
            [[         1, "1"],
             [         2, "2"],
             [         3, "1="],
             [         4, "1-"],
             [         5, "10"],
             [         6, "11"],
             [         7, "12"],
             [         8, "2="],
             [         9, "2-"],
             [        10, "20"],
             [        15, "1=0"],
             [        20, "1-0"],
             [      2022, "1=11-2"],
             [      4890, "2=-1=0"],
             [     12345, "1-0---0"],
             [ 314159265, "1121-1110-1=0"]] ].

part1sample() ->
    Sum = 4890,
    Sum = fuel_sum(sample()),
    Snafu = "2=-1=0",
    Snafu = int_to_snafu(Sum),
    Snafu.


int_to_snafu(Int)->
     int_to_snafu(largest_pos(Int), Int).

%% Int is the current remaining value (may be negative!)
int_to_snafu(1, -2) -> "=";
int_to_snafu(1, -1) -> "-";
int_to_snafu(1, 0) -> "0";
int_to_snafu(1, 1) -> "1";
int_to_snafu(1, 2) -> "2";
int_to_snafu(Pos, Int) ->
    Small = Pos div 5,
    LargestSmaller = largest_num(Small),
    First = if abs(Int) =< LargestSmaller -> 0;
               Int > (Pos + LargestSmaller) -> 2;
               Int > 0 -> 1;
               Int < -(Pos + LargestSmaller) -> -2;
               Int < 0 -> -1
            end,
    Rem = Int - (First * Pos),
    [digit(First) | int_to_snafu(Pos div 5, Rem)].

part1() ->
    Sum = fuel_sum(input()),
    io:format("Fuel sum: ~p~n", [Sum]),
    int_to_snafu(Sum).
