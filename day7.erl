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
     lists:map(fun({dir, P1, _} = Dir) when P == P1 -> update_path(Dir, Path, Fun);
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
build({"cd", "..",[]}, {Cwd, Root}) -> {lists:sublist(Cwd, 1, length(Cwd)-1), Root};
build({"cd", Path, []}, {Cwd, Root}) -> {Cwd ++ [Path], Root};
build({"ls", [], Contents}, {Cwd, Root}) ->
    Conts = [parse_contents(F) || F <- Contents],
    {Cwd, update_path(Root, Cwd,
                      fun({dir,Name,_}) -> {dir,Name,Conts} end)}.

build_model(Input) ->
    {_,Root} = lists:foldl(fun build/2, {undefined, {dir,"",[]}}, Input),
    Root.

print_model(Root) ->
    io:format("- / (dir)~n"),
    print_model(Root, "  ").
print_model({dir, _, Contents}, Prefix) ->
    lists:foreach(fun({file,Name,Size}) ->
                          io:format("~s- ~s (file,size=~p)~n", [Prefix, Name, Size]);
                     ({dir,Name,_} = Dir) ->
                          io:format("~s- ~s (dir)~n", [Prefix, Name]),
                          print_model(Dir, Prefix ++ "  ")
                  end,
                  Contents).

%% Calculate disk usage

du({file,_,Size}) -> Size;
du({dir,_,Contents}) -> lists:sum([du(C) || C <- Contents]).

prewalk(Fun, {file,_,_}=File,Acc) -> Fun(File,Acc);
prewalk(Fun, {dir,_,Contents}=Dir, Acc0) ->
    Acc1 = Fun(Dir,Acc0),
    lists:foldl(fun(C,AccI) -> prewalk(Fun,C,AccI) end, Acc1, Contents).

part1() ->
    prewalk(fun({dir,_,_}=D,Acc) ->
                    Size = du(D),
                    case Size < 100000 of
                        true -> Acc + Size;
                        false -> Acc
                    end;
               (_,Acc) -> Acc
            end, input(), 0).

part2() ->
    Root = input(),
    TotalSpace = 70000000,
    NeededFreeSpace = 30000000,
    ActualFreeSpace = TotalSpace - du(Root),
    MinSizeToDelete = NeededFreeSpace - ActualFreeSpace,
    io:format("Need to find directory to delete with size of: ~p~n", [MinSizeToDelete]),
    prewalk(
      fun({dir,_,_}=D,Acc) ->
              Size = du(D),
              case Size >= MinSizeToDelete of
                  true -> min(Size,Acc);
                  false -> Acc
              end;
         (_,Acc) -> Acc
      end, Root, 576460752303423488).
