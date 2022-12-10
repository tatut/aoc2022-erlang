-module(day10).
-compile(export_all).

input() ->
    [parse_insn(L) || L <- fileutil:lines("day10.txt")].

parse_insn(Line) ->
    Ins = string:split(Line, " "),
    case hd(Ins) of
        "noop" -> noop;
        "addx" -> {addx, list_to_integer(lists:nth(2, Ins))}
    end.

-record(cpu,{ic=1,x=1,acc=[],gather_fun}).

maybe_gather(#cpu{ic=Ic,x=X,acc=Acc,gather_fun=GF}=Cpu) ->
    if (Ic == 20) or ((Ic-20) rem 40 == 0) ->
            Cpu#cpu{acc=[GF(Ic,X)|Acc]};
       true -> Cpu
    end.

insn({addx,N}, #cpu{ic=Ic,x=X}=Cpu0) ->
    Cpu1 = maybe_gather(Cpu0#cpu{ic=Ic+1}),
    maybe_gather(Cpu1#cpu{ic=Ic+2,x=X+N});
insn(noop, #cpu{ic=Ic}=Cpu) ->
    maybe_gather(Cpu#cpu{ic=Ic+1}).

part1() ->
    Cpu0 = #cpu{gather_fun=fun(Ic,X) -> Ic * X end},
    Cpu1 = lists:foldl(fun insn/2, Cpu0, input()),
    lists:sum(Cpu1#cpu.acc).


%%%%% part 2


cycle(#cpu{ic=Ic,x=X,acc=Acc}=Cpu) ->
    %% check if X within cycle
    S = Ic rem 40,
    Draw = if (S >= X) and (S =< X+2) ->
                   $#;
              true -> $.
            end,
    Cpu#cpu{acc=[Draw|Acc]}.

insn2({addx,N}, #cpu{ic=Ic,x=X}=Cpu0) ->
    Cpu1 = cycle(Cpu0),
    Cpu2 = Cpu1#cpu{ic=Ic+1},
    Cpu3 = cycle(Cpu2),
    Cpu3#cpu{ic=Ic+2,x=X+N};
insn2(noop, #cpu{ic=Ic}=Cpu) ->
    Cpu1 = cycle(Cpu),
    Cpu1#cpu{ic=Ic+1}.

part2() ->
    #cpu{acc=Acc} = lists:foldl(fun insn2/2, #cpu{ic=1}, input()),
    draw(lists:reverse(Acc)).

draw([]) -> ok;
draw(Str) ->
    {Line,Rest} = lists:split(40, Str),
    io:format("~s~n", [Line]),
    draw(Rest).
