-module(fileutil).
-export([lines/1]).

%% Read text files line at a time

lines(File) ->
    {ok, S} = file:open(File, read),
    Lines = read_lines(S,[]),
    file:close(S),
    Lines.

read_lines(S, Lines) ->
    case io:get_line(S,'') of
        eof -> lists:reverse(Lines);
        Line -> read_lines(S, [string:trim(Line)|Lines])
    end.


binary:split(${1:Arg1}, ${2:Arg2}, ${3:Arg3})
