-module(day7).
-compile(export_all).

input() ->
    build_model(cmds(fileutil:lines("day7.txt"), [])).

%% Split into list of {Cmd, Argument, OutputLines}
cmds([], Acc) -> lists:reverse(Acc);
cmds([[$$, $ | CmdLine]|Rest], Acc) ->
    {Output, Rest1} = lists:splitwith(fun([$$, $  | _]) -> false; (_) -> true end, Rest),
    {Cmd, Arg} = case string:split(CmdLine, " ") of
                     [C] -> {C, ""};
                     [C,A] -> {C,A}
                 end,
    cmds(Rest1, [{Cmd, Arg, Output} | Acc]).

%% Build model from commands
%% model is a tree of dirs/files
%% and a path to the current working directory
%%
%% {dir, Name, Contents}
%% {file, Name, Size}
%%
%% Cwd is list of directory names ["a", "e"] from the root
%% root is {dir, "", Contents}

update_path(Root, [], Fun) -> Fun(Root);
update_path({dir,Name,Contents}, [P | Path], Fun) ->
    {dir, Name,
     lists:map(fun({dir, PathName, _} = Dir) ->
                       case PathName == P of
                           true -> update_path(Dir, Path, Fun);
                           false -> Dir
                       end;
                  (Else) -> Else end,
               Contents)}.

parse_contents(X) ->
    [DirOrSize, Name] = string:split(X, " "),
    case DirOrSize of
        "dir" -> {dir, Name, []};
        Size -> {file, Name, list_to_integer(Size)}
    end.

p(Path) -> "/" ++ string:join(Path, "/").

build({"cd", "/", []}, {_, Root}) -> {[], Root};
build({"cd", "..",[]}, {Cwd, Root}) ->
    NewCwd = lists:sublist(Cwd, 1, length(Cwd)-1),
    %%io:format("  CD ~p => ~p~n", [p(Cwd), p(NewCwd)]),
    {NewCwd, Root};
build({"cd", Path, []}, {Cwd, Root}) ->
    NewCwd = Cwd ++ [Path],
    %%io:format("  CD ~p => ~p~n", [p(Cwd), p(NewCwd)]),
    {NewCwd, Root};
build({"ls", [], Contents}, {Cwd, Root}) ->
    Conts = [parse_contents(F) || F <- Contents],
    %%io:format("  CWD ~p HAS ~p~n", [p(Cwd), Conts]),
    {Cwd, update_path(Root, Cwd, fun({dir,Name,_}) -> {dir,Name,Conts} end)}.

build_model(Input) ->
    {_,Root} = lists:foldl(fun(Cmd,Acc) ->
                                   %%io:format("AT: ~p RUN: ~p~n", [element(1,Acc),Cmd]),
                                   Acc1 = build(Cmd,Acc),
                                   %%io:format("  AFTER: ~p~n\n---------\n", [Acc1]),
                                   Acc1

                           end, {undefined, {dir,"",[]}}, Input),
    Root.

print_model(Root) ->
    io:format("- / (dir)~n"),
    print_model(Root, "  ").
print_model({dir, _, Contents}, Prefix) ->
    lists:foreach(fun({file,Name,Size}) ->
                          io:format("~s- ~s (file,size=~p)~n", [Prefix, Name, Size]);
                     ({dir,Name,_} = Dir) ->
                          io:format("~s- ~s (dir)~n", [Prefix, Name]),
                          print_model(Dir, Prefix ++ "  ");
                     (Else) -> io:format("~s  SOMETHING ELSE ~p~n", [Prefix, Else])
                  end,
                  Contents).

%% Calculate disk usage

du({file,_,Size}) -> Size;
du({dir,_,Contents}) -> lists:sum([du(C) || C <- Contents]).

prewalk(Fun, {file,_,_}=File) -> Fun(File);
prewalk(Fun, {dir,_,Contents}=Dir) ->
    Fun(Dir),
    lists:foreach(fun(C)->prewalk(Fun,C) end, Contents).

receive_numbers(Acc) ->
    receive
        N -> receive_numbers(Acc + N)
    after
        1000 -> Acc
    end.


part1() ->
    Me = self(),
    Root = input(),

    %% Spawn process to go thru the tree and report back
    spawn(fun() ->
                  prewalk(fun({dir,_,_}=D) ->
                                  Size = du(D),
                                  case Size < 100000 of
                                      true -> Me ! Size;
                                      false -> ok
                                  end;
                             (_) -> ok
                          end, Root) end),
    receive_numbers(0).

receive_smallest() ->
    receive N ->
            receive_smallest(N)
    end.
receive_smallest(Acc) ->
    receive
        N -> receive_smallest(min(N,Acc))
    after
        1000 -> Acc
    end.

part2() ->
    Root = input(),
    TotalSpace = 70000000,
    NeededFreeSpace = 30000000,
    ActualFreeSpace = TotalSpace - du(Root),
    MinSizeToDelete = NeededFreeSpace - ActualFreeSpace,
    io:format("Need to find directory to delete with size of: ~p~n", [MinSizeToDelete]),
    Me = self(),
    spawn(fun() ->
                  prewalk(fun({dir,_,_}=D) ->
                                  Size = du(D),
                                  case Size >= MinSizeToDelete of
                                      true -> Me ! Size;
                                      false -> ok
                                  end;
                             (_) -> ok
                          end, Root)
          end),
    receive_smallest().
