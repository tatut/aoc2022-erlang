-module(fileutil).
-export([lines/1, lines/2]).

%% Read text files line at a time

lines(File) ->
    {ok, S} = file:open(File, read),
    Lines = read_lines(S,[]),
    file:close(S),
    Lines.

lines(File, Regex) ->
    {ok, RE} = re:compile(Regex),
    [ matches(L, RE) || L <- lines(File) ].

matches(Str, RE) ->
    {match, [_ | Groups]} = re:run(Str, RE),
    list_to_tuple([ string:slice(Str, Gs, Gl) || {Gs, Gl} <- Groups ]).

read_lines(S, Lines) ->
    case io:get_line(S,'') of
        eof -> lists:reverse(Lines);
        Line -> read_lines(S, [string:trim(Line)|Lines])
    end.
