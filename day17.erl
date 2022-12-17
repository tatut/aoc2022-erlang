-module(day17).
-compile(export_all).


%% Parse input jets
input() ->
    {ok, Bin} = file:read_file("day17.txt"),
    lists:flatmap(fun(P) ->
                          case binary:at(Bin, P) of
                              $< -> [-1];
                              $> -> [1];
                              _ -> []
                          end
                  end,
                  lists:seq(0, size(Bin)-1)).

%% Returns a rotation state that loops over the list
rotation(List) ->
    {rotation, List, List}.

next({rotation,All, [I|Items]}) ->
    {I, {rotation, All, Items}};
next({rotation, All, []}) ->
    {hd(All), {rotation, All, tl(All)}}.

%% Define the shapes as list of points {X,Y} points

%% ####
-define(DASH, [{0,0},{1,0},{2,0},{3,0}]).

%% .#.
%% ###
%% .#.
-define(PLUS, [{1,0},
               {0,1},{1,1},{2,1},
               {1,2}]).

%% ..#
%% ..#
%% ###
-define(REVL, [{2,0},
               {2,1},
               {0,2},{1,2},{2,2}]).

%% #
%% #
%% #
%% #
%% call this block TALL because LINE constant is predefined and can't be used
-define(TALL, [{0,0},
               {0,1},
               {0,2},
               {0,3}]).

%% ##
%% ##
-define(BLOK, [{0,0}, {1,0},
               {0,1}, {1,1}]).

%% Cave width units
-define(FLOOR, 10000000). %% FIXME: make this BIG for actual

%% We keep track of the board as an ETS table that
%% sparsely stores all points that have rock in them
%% as well as the current top
empty_board() ->
    T = ets:new(day17, [set]),
    ets:insert(T, {top, ?FLOOR}),
    T.

piece_height(?DASH) -> 1;
piece_height(?PLUS) -> 3;
piece_height(?REVL) -> 3;
piece_height(?TALL) -> 4;
piece_height(?BLOK) -> 2.

translate(Piece, Dx, Dy) ->
    [{X+Dx,Y+Dy} || {X,Y} <- Piece].

%% Take the current top and a piece
%% translate piece coordinates into where it appears.
appear(Top, Piece) ->
    translate(Piece, 2, Top - (3 + piece_height(Piece))).

top(T) ->
    [{_, Top}] = ets:lookup(T, top),
    Top.

at(T, Pos) ->
    case ets:lookup(T,Pos) of
        [{_, Ch}] -> Ch;
        _ -> $.
    end.

available(T, Pos) ->
    at(T,Pos) == $..


%% Try to move, returns new piece or false if movement
%% is blocked for any reason
try_move(T, Piece, Dx, Dy) ->
    NewPiece = translate(Piece, Dx, Dy),
    Ok = lists:all(fun({X,_}) when X < 0 orelse X > 6 -> false;
                      ({_,Y}) when Y >= ?FLOOR -> false;
                      (Pos) -> available(T,Pos)
                   end, NewPiece),
    case Ok of
        true -> NewPiece;
        _ -> false
    end.

land(T, Piece) ->
    OldTop = top(T),
    [ ets:insert(T, {Pos, $#}) || Pos <- Piece ],
    NewTop = min(OldTop, lists:min([Y || {_,Y} <- Piece])),
    ets:insert(T, {top, NewTop}).

draw(T, Piece0, Round, Iter) ->
    xpm:draw(
      io_lib:format("day17_~6.10.0b_~6.10.0b.xpm", [Round,Iter]),
      {8, 25},
      [{$., {255,255,255}},
       {$#, {0,0,0}},
       {$@, {255,50,50}},
       {$|, {0,0,255}},
       {$-, {0,255,0}}],
      T,
      fun({7,_}) -> $|;
         ({_,?FLOOR}) -> $-;
         (Pos) ->
              case lists:member(Pos, Piece0) of
                  true -> $@;
                  false -> $.
              end
      end).

%% Drop a piece
drop(T, Piece0, JetsRot0, Round, Iter) ->
    %%draw(T,Piece0,Round,Iter),
    %% Move by jet first
    {Jet, JetsRot1} = next(JetsRot0),
    Piece1 = case try_move(T, Piece0, Jet, 0) of
                 false -> Piece0;
                 NewPiece -> NewPiece
             end,
    %%draw(T,Piece1,Round,Iter+1),
    %% Try to move by dropping, if fails, land here
    case try_move(T, Piece1, 0, 1) of
        false -> land(T, Piece1), JetsRot1;
        Piece2 -> drop(T, Piece2, JetsRot1, Round, Iter+2)
    end.


simulate(_, _, _, Round, MaxRounds) when Round > MaxRounds -> done;
simulate(T, PieceRot0, JetsRot0, Round, MaxRounds) ->
    {NextPiece, PieceRot1} = next(PieceRot0),
    JetsRot1 = drop(T, appear(top(T), NextPiece), JetsRot0, Round, 1),
    simulate(T, PieceRot1, JetsRot1, Round+1, MaxRounds).




part1() ->
    T = empty_board(),
    PieceRot = rotation([?DASH, ?PLUS, ?REVL, ?TALL, ?BLOK]),
    JetsRot = rotation(input()),
    simulate(T, PieceRot, JetsRot, 1, 2022),
    ?FLOOR - top(T).
