-module(listutil).
-compile(export_all).

splitgroups(Separator, Lst) ->
    splitgroups([], Separator, Lst).

splitgroups(Acc, _, []) -> lists:reverse(Acc);
splitgroups(Acc, Separator, Lst) ->
    {Before, After} = lists:splitwith(fun(X) -> X =/= Separator end, Lst),
    case After of
        [] -> lists:reverse([Before | Acc]);
        [Separator | Rest] ->
            splitgroups([Before | Acc], Separator, Rest)
    end.
