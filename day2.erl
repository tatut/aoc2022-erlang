-module(day2).
-compile(export_all).

-type move() :: rock | paper | scissors.
-type winner() :: opponent | me | tie.

-spec move(integer()) -> move().
move($A) -> rock;
move($B) -> paper;
move($C) -> scissors;
move($X) -> rock;
move($Y) -> paper;
move($Z) -> scissors.

-spec winner(move(), move()) -> winner().
winner(X,X) -> tie;
winner(rock,paper) -> me;
winner(rock,scissors) -> opponent;
winner(paper,scissors) -> me;
winner(paper,rock) -> opponent;
winner(scissors,rock) -> me;
winner(scissors,paper) -> opponent.

-spec move_score(move()) -> integer().
move_score(rock) -> 1;
move_score(paper) -> 2;
move_score(scissors) -> 3.

-spec my_score(move(), move()) -> integer().
my_score(OpponentMove, MyMove) ->
    move_score(MyMove) +
        case winner(OpponentMove, MyMove) of
            opponent -> 0;
            tie -> 3;
            me -> 6
        end.

-spec play_round1(binary()) -> integer().
play_round1(<<>>) -> 0;
play_round1(<<Opponent:8, " ", Me:8>>) ->
    my_score(move(Opponent), move(Me)).

play(Input, RoundFun) ->
    lists:foldr(fun(Round,Acc) -> Acc + RoundFun(Round) end,
                0,
                binary:split(Input, <<"\n">>, [global])).

part1() ->
    {ok, Input} = file:read_file("day2.txt"),
    play(Input, fun play_round1/1).


%%%%%%%%%%
% Part 2: second character is instructions

%% Y means tie
determine_move($Y, OpponentMove) -> OpponentMove;
%% X means you must lose
determine_move($X, rock) -> scissors;
determine_move($X, paper) -> rock;
determine_move($X, scissors) -> paper;
%% Z means you must win
determine_move($Z, rock) -> paper;
determine_move($Z, paper) -> scissors;
determine_move($Z, scissors) -> rock.

play_round2(<<>>) -> 0;
play_round2(<<Opponent:8, " ", Me:8>>) ->
    OpponentMove = move(Opponent),
    my_score(OpponentMove, determine_move(Me, OpponentMove)).

part2() ->
    {ok, Input} = file:read_file("day2.txt"),
    play(Input, fun play_round2/1).
