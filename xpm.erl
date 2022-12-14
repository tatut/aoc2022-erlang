-module(xpm).
-compile(export_all).

xpm_col(Ch,{R,G,B}) ->
    Hex = io_lib:format("~2.16.0b~2.16.0b~2.16.0b",[R,G,B]),
    ["\"", Ch, " c #", Hex, "\",\n"].

xpm_head() -> <<"/* XPM */\nstatic char * XFACE[] = {\n">>.

xpm_info(W,H,Colors) ->
    ["\"", integer_to_list(W), " ", integer_to_list(H), " ", integer_to_list(length(Colors)), " 1\",\n",
     [ xpm_col(Ch, Col) || {Ch, Col} <- Colors ]].

xpm_foot() -> "};".

xpm_image_line(Y, W, H, Pix, Default) ->
    ["\"",
     [ case ets:lookup(Pix, {X,Y}) of
           [{_,Char}] -> Char;
           _ -> case Default of
                    F when is_function(Default) -> F({X,Y});
                    _ -> Default
                end
       end || X <- lists:seq(0, W) ],
     "\"",
     if Y == H -> ",\n";
        true -> "\n"
     end
    ].


draw(File, {MaxX,MaxY}, Colors, PixData, Default) ->
    W = MaxX+1, H = MaxY+1,
    io:format("Write xpm to ~p (~px~p)~n", [File, W, H]),
    file:write_file(File,
                    [xpm_head(),
                     xpm_info(W, H, Colors),
                     [ xpm_image_line(Y, W, H, PixData, Default) || Y <- lists:seq(0, H) ],
                     xpm_foot()]).

draw(File, Colors, PixData, Default) ->
    Size = ets:foldl(fun({{X,Y},_}, {Mx,My}) -> {max(X,Mx), max(Y,My)};
                               (_, Acc) -> Acc
                            end,
                            {0,0}, PixData),
    draw(File, Size, Colors, PixData, Default).
