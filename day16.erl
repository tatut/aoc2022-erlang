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


%% Traveling salesman problem, take all possible permutations of the order, calculate how much it
%% will relieve.

-record(actor, {at="AA", path=[]}).
-record(route, {g, time=30, tpr=0, nodes,
                elf=#actor{},
                elephant=undefined}).

pressure(G) ->
    lists:sum([ case digraph:vertex(G, V) of
                    {_, #valve{open=true, rate=R}} -> R;
                    _ -> 0
                end ||
                  V <- digraph:vertices(G) ]).

update_actor(_, undefined, Nodes) -> {undefined,Nodes};
update_actor(G, #actor{at=At, path=[]}, Nodes) ->
    %% Path exhausted, open this valve and select a new path
    {_,OldV} = digraph:vertex(G, At),
    digraph:add_vertex(G, At, OldV#valve{open = true}),
    %% Calculate next path step route to next
    case Nodes of
        [] -> {undefined, []};
        [N|Rest] ->
            %% Calculate path to next target valve
            [_|Path] = digraph:get_short_path(G, At, N),
            {#actor{at=At, path=Path}, Rest}
    end;
update_actor(_, #actor{path=[To|Path]}, Nodes) ->
    %% Traveling to target
    {#actor{at=To, path=Path}, Nodes}.

route(#route{time=0, tpr=Tpr}) -> Tpr; % time is up, just return the TPR
route(#route{g=G, elf=Elf0, elephant=Elephant0, time=T, nodes=NodesIn, tpr=Tpr}=R) ->
    NextTpr = Tpr + pressure(G),

    {Elf,Nodes0} = update_actor(G, Elf0, NodesIn),
    {Elephant, Nodes1} = update_actor(G, Elephant0, Nodes0),

    route(R#route{elf=Elf, elephant=Elephant,
                  time=T-1, tpr=NextTpr, nodes=Nodes1}).



close_all_valves(G) ->
    [ digraph:add_vertex(G, V, (element(2,digraph:vertex(G, V)))#valve{open = false})
      || V <- digraph:vertices(G) ].

execute_route(G, Time, Elf, Elephant, Nodes) ->
    close_all_valves(G),
    %% Add 1 to time, so actors take their first routes at that time
    route(#route{g=G, time = Time+1, elf=Elf, elephant=Elephant, nodes=Nodes}).

%% sample best route 1651
%%execute_route(G, ["DD", "BB", "JJ", "HH", "EE", "CC"]).

work(Name) ->
    N = 8,
    lists:foreach(fun(_) -> spawn(?MODULE, Name, [self()]) end,
                  lists:seq(1, N)),
    receive_result(0).

part1() -> work(part1_worker).
part2() -> work(part2_worker).

receive_result(Max) ->
    receive
        X when X > Max ->
            io:format("New MAX: ~p~n",[X]),
            receive_result(X);
        _ -> receive_result(Max)
    after 10000 ->
            Max
    end.

worker_with(ResultPid, MakeExecute) ->
    G = input(),
    OtherValves = [ V || V <- digraph:vertices(G),
                                  V =/= "AA" ] ,
    OnlyUseful = shuffle(lists:sort(fun(V1,V2) ->
                                    {_, #valve{rate=R1}} = digraph:vertex(G,V1),
                                    {_, #valve{rate=R2}} = digraph:vertex(G,V2),
                                    R1 > R2
                            end,
                            (lists:filter(fun(V) ->
                                                  {_, #valve{rate = Rate}} = digraph:vertex(G, V),
                                                  Rate > 0 end, OtherValves)))),
    solve(ResultPid, OnlyUseful, 0, MakeExecute(G)).

part1_worker(ResultPid) ->
    worker_with(ResultPid, fun(G) ->
                                   fun(Route) -> execute_route(G, 30, #actor{}, undefined, Route) end
                           end).

part2_worker(ResultPid) ->
    worker_with(ResultPid, fun(G) ->
                                   fun(Route) -> execute_route(G, 26, #actor{}, #actor{}, Route) end
                           end).

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


solve(ResultPid, Valves, Tpr0, Execute) ->
    Swapped = swap2(Valves),
    SwappedTpr = Execute(Swapped),
    if SwappedTpr > Tpr0 ->
            ResultPid ! SwappedTpr,
            solve(ResultPid, Swapped, SwappedTpr, Execute);
       true -> solve(ResultPid, Valves, Tpr0, Execute)
    end.


%% All permutations work on the sample input, but on on actual input, there are 54! possible permutations
%% of valve orders to check
