-module(day19).
-compile(export_all).

-record(robot, {type :: atom(), cost :: [{integer(),atom()}]}).
-record(blueprint, {id, robots, costs}).
-record(state, {minute = 0 :: integer(),
                blueprint :: #blueprint{},
                robots = #{ore => 1, clay => 0, obsidian => 0, geode => 0} :: #{atom() => integer()},
                materials = #{ore => 0, clay => 0, obsidian => 0, geode => 0}
                :: #{atom() => integer()}}).

input() ->
    [ calculate_costs(#blueprint{id=list_to_integer(Id),
                                 robots=parse_robots(Robots, #{})}) ||
        {Id, Robots} <- fileutil:lines("day19.txt", "^Blueprint (\\d+): (.*)$")].

calculate_costs(#blueprint{robots=R}=Bp) ->
    CostMap = maps:fold(fun(Type, _, Acc) ->
                                maps:put(Type, cost_of(Type, Bp), Acc)
                        end,
                        #{}, R),
    Bp#blueprint{costs = CostMap}.

cost_of(ore, _) -> 1;
cost_of(Type, #blueprint{robots=R}=Bp) ->
    #robot{cost=Cost} = maps:get(Type, R),
    lists:sum([ Amount * cost_of(Type1, Bp)  || {Amount,Type1} <- Cost]).

re(Str) ->
    {match, [_ | Groups]} = re:run(Str, " ?Each (\\w+) robot costs (\\d+) (\\w+)( and (\\d+) (\\w+))?"),
    [string:slice(Str, Gs, Gl) || {Gs, Gl} <- Groups].

parse_robots([""], Robots) -> Robots;
parse_robots(Str, Robots) ->
    [Spec | Rest] = string:split(Str, "."),
    Robot = case re(Spec) of
                [Type, Cost, Material] ->
                    #robot{type=list_to_atom(Type),
                           cost=[{list_to_integer(Cost), list_to_atom(Material)}]};
                [Type, Cost1, Material1, _, Cost2, Material2] ->
                    #robot{type=list_to_atom(Type),
                           cost=[{list_to_integer(Cost1), list_to_atom(Material1)},
                                 {list_to_integer(Cost2), list_to_atom(Material2)}]}
            end,
    parse_robots(Rest, maps:put(Robot#robot.type, Robot, Robots)).

mat(Type,#state{materials=Mat}) -> maps:get(Type, Mat).
incmat(Type,By,#state{materials=Mat}=S) -> S#state{materials=maps:put(Type, By+mat(Type,S), Mat)}.
rob(Type, #state{robots=Robots}) -> maps:get(Type, Robots).

simulate(#state{minute=Min, robots = Robots}=S0, BuildType) ->
    S1 = case BuildType of
             none -> S0;
             Type -> build(Type, S0)
         end,
    case S1 of
        undefined -> undefined;
        _ ->
            %% Build succeeded, accumulate materials
            S2 = maps:fold(fun(Type, Count, S) -> incmat(Type, Count, S) end, S1, Robots),
            S2#state{minute=Min+1}
    end.

build(Type, #state{blueprint = Blueprint, materials=Materials, robots=R}=S) ->
    #robot{cost = Cost} = maps:get(Type, Blueprint#blueprint.robots),
    case lists:all(fun({Amount,Material}) -> mat(Material, S) >= Amount end, Cost) of
        true ->
            %%io:format("enough materials to make ~p robot ~p~n", [Type, Cost]),
            S#state{robots=maps:put(Type, 1+maps:get(Type,R), R),
                    materials=lists:foldl(fun({Amount,Material}, M) ->
                                                  maps:put(Material, maps:get(Material,M)-Amount, M)
                                          end, Materials, Cost)};
        false -> undefined %% not enough materials
    end.

%% Simulate all states, one where we try to build each type of robot
%% and one where we don't build anything
simulate_states(States) ->
    [ S || S <- [ simulate(State, BuildType) || State <- States,
                                                BuildType <- [none, ore, clay, obsidian, geode] ],
           S =/= undefined ].


value(#state{blueprint=#blueprint{costs = Costs}}=State) ->
    lists:sum([ (rob(T, State)+mat(T,State)) * maps:get(T,Costs)
                || T <- [geode,obsidian,clay,ore] ]).

simulate_rounds(States, Minutes, MaxMinutes) when Minutes == MaxMinutes  -> States;
simulate_rounds(States, Minutes, MaxMinutes) ->
    %% We need to trim the states, so that the amount is manageable
    %% sort by the value of the state and take top 1000
    TrimmedStates = lists:sublist(lists:sort(fun(S1,S2) -> value(S1) > value(S2) end, States), 1000),
    simulate_rounds(simulate_states(TrimmedStates), Minutes + 1, MaxMinutes).

simulate_blueprint(Blueprint, MaxMinutes) ->
    FinishedStates = simulate_rounds([#state{blueprint = Blueprint}], 0, MaxMinutes),
    BestState = hd(lists:sort(fun(S1, S2) ->
                                      mat(geode,S1) > mat(geode, S2) end,
                              FinishedStates)),
    Id = Blueprint#blueprint.id,
    Geodes = mat(geode,BestState),
    Ql = Id * mat(geode,BestState),
    {Geodes, Ql}.


part1() ->
    Blueprints = input(),
    lists:sum([ Ql || {_, Ql} <- [ simulate_blueprint(Bp, 24) || Bp <- Blueprints ]]).

part2() ->
    Blueprints = lists:sublist(input(), 3),
    lists:foldl(fun(Bp,Acc) ->
                        {Geodes,_} = simulate_blueprint(Bp, 32),
                        Acc * Geodes
                end, 1, Blueprints).
