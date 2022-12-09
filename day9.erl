-module(day9).
-compile(export_all).

parse_move(M) ->
    [Dir,Count] = string:split(M," "),
    {list_to_atom(Dir), list_to_integer(Count)}.

input() ->
    [parse_move(L) || L <- fileutil:lines("day9.txt")].

-record(state, {head, tail, visited}).

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

adjust_tail({Hx,Hy}, {Tx,Ty}) ->
    Dx = Hx-Tx,
    Dy = Hy-Ty,
    if
        %% Still touching, do nothing
        (abs(Dx) < 2) and (abs(Dy) < 2) -> {Tx,Ty};

        true -> {Tx+sign(Dx), Ty+sign(Dy)}
    end.

next_state(#state{tail=OldT, visited=V}, NewH) ->
    NewT = adjust_tail(NewH, OldT),
    #state{head=NewH, tail=NewT, visited=sets:add_element(NewT, V)}.

move({Dir, Count}, State) ->
    if Count > 0 -> move({Dir, Count - 1}, move(Dir, State));
       true -> State
    end;
move('U', #state{head={Hx,Hy}}=S) -> next_state(S, {Hx,Hy-1});
move('D', #state{head={Hx,Hy}}=S) -> next_state(S, {Hx,Hy+1});
move('L', #state{head={Hx,Hy}}=S) -> next_state(S, {Hx-1,Hy});
move('R', #state{head={Hx,Hy}}=S) -> next_state(S, {Hx+1,Hy}).

part1() ->
    P = {0,0},
    S = lists:foldl(fun move/2,
                    #state{head=P,tail=P,visited=sets:from_list([P])},
                    input()),
    sets:size(S#state.visited).



move_rope({Dir,Count}, State) ->
    if Count > 0 -> move_rope({Dir,Count-1}, move_rope(Dir, State));
       true -> State
    end;
move_rope('U', State) -> next_rope_state(State, 0, -1);
move_rope('D', State) -> next_rope_state(State, 0, 1);
move_rope('L', State) -> next_rope_state(State, -1, 0);
move_rope('R', State) -> next_rope_state(State, 1, 0).

next_rope_state({[{Hx,Hy}|Tails], Visited}, MoveX, MoveY) ->
    NewH = {Hx+MoveX,Hy+MoveY},
    NextRope = next_rope(NewH, Tails, [NewH]),
    {NextRope, sets:add_element(lists:last(NextRope), Visited)}.

next_rope(NewH, [OldT], Acc) ->
    lists:reverse([adjust_tail(NewH, OldT) | Acc]);
next_rope(NewH, [OldT|Tails], Acc) ->
    NewT = adjust_tail(NewH, OldT),
    next_rope(NewT, Tails, [NewT | Acc]).


part2() ->
    %% there are 9 tails
    Rope = [{0,0} || _ <- lists:seq(1,10)],
    {_, V} = lists:foldl(fun move_rope/2,
                         {Rope, sets:new()},
                         input()),
    sets:size(V).
