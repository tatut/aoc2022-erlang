-module(day16).
-compile(export_all).
-record(valve, {id, rate, to, open=false}).

input() ->
    Valves =
        [valve(L) ||
            L <- fileutil:lines("day16.txt",
                                "^Valve (..) has flow rate=(\\d+); tunnels? leads? to valves? (.*)$")],
    build_graph(Valves).

valve({Id, Rate, To}) ->
    #valve{id=Id, rate=list_to_integer(Rate), to=string:split(To, ", ", all)}.

build_graph(Valves) ->
    G = digraph:new(),
    %% Create vertex for each valve
    [ digraph:add_vertex(G, V#valve.id, V) || V <- Valves ],

    %% Create edges for all tunnels
    [ digraph:add_edge(G, V1#valve.id, ToId) ||
        V1 <- Valves, ToId <- V1#valve.to ],

    G.

%% total possible TPR is calculated from current time remaining, and the path to a valve and its rate
possible_tpr(TimeRemaining, PathTo, #valve{rate=Rate}=V) ->
    io:format("~p possible_tpr T=~p, Path=~p  => ~p~n", [V, TimeRemaining, PathTo, Rate * (TimeRemaining - length(PathTo) - 1)]),
    io:format("  pressure mins ~p - ~p - 2 => ~p", [TimeRemaining, length(PathTo), (TimeRemaining - (length(PathTo) - 1) - 1)]),
    Rate * (TimeRemaining - (length(PathTo) - 1) - 1).


%% all permutations
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%% Traveling salesman problem, take all possible permutations of the order, calculate how much it
%% will relieve.

%%-record(actor, {at="AA", path=[]}).
-record(route, {g, time=30, at="AA", path=[], nodes, tpr=0}).

pressure(G) ->
    lists:sum([ case digraph:vertex(G, V) of
                    {_, #valve{open=true, rate=R}} -> R;
                    _ -> 0
                end ||
                  V <- digraph:vertices(G) ]).

route(#route{time=0, tpr=Tpr}) -> Tpr; % time is up, just return the TPR
route(#route{g=G, at=At, path=[], time=T, nodes=Nodes, tpr=Tpr}=R) ->
    NextTpr = Tpr + pressure(G),
    %% Exhausted a path, open the current node
    {_,OldV} = digraph:vertex(G, At),
    digraph:add_vertex(G, At, OldV#valve{open = true}),
    %% Calculate next path step route to next
    case Nodes of
        [] -> route(R#route{time=T-1, tpr=NextTpr});
        [N|Rest] ->
            %% Calculate path to next target valve
            [_|Path] = digraph:get_short_path(G, At, N),
            route(R#route{at=At, time=T-1, path=Path, nodes=Rest, tpr=NextTpr})
    end;
route(#route{g=G, path=[To|Path], time=T, tpr=Tpr}=R) ->
    %% Going to the next valve
    NextTpr = Tpr + pressure(G),
    %%io:format("  move to ~p , pressure released = ~p~n", [To, pressure(G)]),
    route(R#route{at=To, path=Path, time=T-1, tpr=NextTpr}).


close_all_valves(G) ->
    [ digraph:add_vertex(G, V, (element(2,digraph:vertex(G, V)))#valve{open = false})
      || V <- digraph:vertices(G) ].

execute_route(G, [FirstNode|Nodes]) ->
    close_all_valves(G),
    [_|Path] = digraph:get_short_path(G, "AA", FirstNode),
    %%io:format("Exec route: ~p~n", [[FirstNode|Nodes]]),
    route(#route{g=G, path=Path, nodes=Nodes}).
    %%io:format("   TPR -> ~p~n", [TPR]).

%% sample best route 1651
%%execute_route(G, ["DD", "BB", "JJ", "HH", "EE", "CC"]).

part1() ->
    N = 8,
    lists:foreach(fun(_) -> spawn(?MODULE, part1_worker, [self()]) end,
                  lists:seq(1, N)),
    receive_result(0).

receive_result(Max) ->
    receive
        X when X > Max ->
            io:format("New MAX: ~p~n",[X]),
            receive_result(X);
        _ -> receive_result(Max)
    after 10000 ->
            Max
    end.

part1_worker(ResultPid) ->
    G = input(),
    OtherValves = [ V || V <- digraph:vertices(G),
                                  V =/= "AA" ] ,
    OnlyUseful = lists:sort(fun(V1,V2) ->
                                    {_, #valve{rate=R1}} = digraph:vertex(G,V1),
                                    {_, #valve{rate=R2}} = digraph:vertex(G,V2),
                                    R1 > R2
                            end,
                            (lists:filter(fun(V) ->
                                                  {_, #valve{rate = Rate}} = digraph:vertex(G, V),
                                                  Rate > 0 end, OtherValves))),
    io:format("length useful ~p~n", [length(OnlyUseful)]),
    part1(ResultPid, G, OnlyUseful, execute_route(G, OnlyUseful)).

shuffle([]) -> [];
shuffle(List) ->
    E = lists:nth(rand:uniform(length(List)), List),
    [E | shuffle(lists:delete(E, List))].

swap2(Valves) ->
    V = list_to_tuple(Valves),
    P1 = rand:uniform(size(V)),
    P2 = rand:uniform(size(V)),
    Val1 = element(P1, V),
    Val2 = element(P2, V),
    tuple_to_list(setelement(P1, setelement(P2, V, Val1), Val2)).

-spec part1(_, _, [], _) -> any().
part1(_, _, [], TprMax) -> TprMax;
part1(ResultPid, G, Valves, Tpr0) ->
    Swapped = swap2(Valves),
    SwappedTpr = execute_route(G, Swapped),
    if SwappedTpr > Tpr0 ->
            ResultPid ! SwappedTpr,
            part1(ResultPid, G, Swapped, SwappedTpr);
       true -> part1(ResultPid, G, Valves, Tpr0)
    end.


%% All permutations work on the sample input, but on on actual input, there are 54! possible permutations
%% of valve orders to check
