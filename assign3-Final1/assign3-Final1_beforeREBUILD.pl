:- initialization main, halt.
:- dynamic white/2.
:- dynamic black/2.
:- table relativeTo/4.
:- dynamic try/2.
:- dynamic try2/2.

read_predicates([H|T]) :-
    read_line_to_codes(user_input, H), 
    H \= end_of_file,
    string_codes(Input, H),
    term_string(Term, Input),
    assert(Term), % 將 Term assert 進 KB
    read_predicates(T).
read_predicates([]).

isEmpty( X, Y ):-
    \+white( X, Y ),
    \+black( X, Y )
.

isValid( X, Y ):-
    X > 0, X <16,
    Y > 0, Y <16
.

% wildcard
relativeTo( 0, 0, _, _ ).
relativeTo( X1, Y1, X2, Y2 ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1,
    isValid( X2, Y2 )
.
relativeTo( X1, Y1, X2, Y2 ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1,
    Y2 is Y1 + Offset,
    isValid( X2, Y2 )
.
relativeTo( X1, Y1, X2, Y2 ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1 + Offset,
    isValid( X2, Y2 )
.
relativeTo( X1, Y1, X2, Y2 ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1 - Offset,
    isValid( X2, Y2 )
.

addMoves( [H|T] ):-
    assert(H),
    addMoves( T )
.
addMoves( [] ).

undoMoves( [H|T] ):-
    retract(H),
    undoMoves( T )
.
undoMoves( [] ).

addMove( Player, X, Y ):-
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    assert(Term)
.

undoMove( Player, X, Y ):-
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    retract(Term)
.

writeMove( Player, X, Y):-
    write(Player), write("("), write(X), write(", "), write(Y), write(")\n")
.

writeMoves( Player, [[X,Y]|T] ):-
    writeMove(Player, X, Y),
    writeMoves( Player, T )
.
writeMoves( _, []).


% N == 1 : 直
directionType( 1, X1, _, X2, _ ):-
    XD is X1 - X2, XD = 0
.
% N == 2 : 橫
directionType( 2, _, Y1, _, Y2 ):-
    YD is Y1 - Y2, YD = 0
.
% N == 3 : 右上左下
directionType( 3, X1, Y1, X2, Y2 ):-
    XD is X1 - X2, YD is Y1 - Y2, XD is YD 
.
% N == 4 : 左上右下
directionType( 4, X1, Y1, X2, Y2 ):-
    XD is X1 - X2, YD is Y1 - Y2, XD is -YD 
.


%nOutOf5( N, Player, Holes ):-
fourOutOfFive( Player, L , H):-
    call( Player, X1, Y1),

    relativeTo( X1, Y1, X2, Y2),
    (X2 > X1 ; (X1 = X2, Y2 > Y1) ),
    call( Player, X2, Y2),
    directionType( N, X1, Y1, X2, Y2),

    relativeTo( X1, Y1, X3, Y3),
    (X3 > X2 ; (X2 = X3, Y3 > Y2)),
    call( Player, X3, Y3),
    directionType( N, X1, Y1, X3, Y3),

    relativeTo( X1, Y1, X4, Y4),
    (X4 > X3 ; (X3 = X4, Y4 > Y3)),
    call( Player, X4, Y4),
    directionType( N, X1, Y1, X4, Y4),

    relativeTo( X1, Y1, HX1, HY1 ),
    relativeTo( X4, Y4, HX1, HY1 ),
    directionType( N , X1, Y1, HX1, HY1),
    validForP( Player, HX1, HY1 ),

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    list_to_set( [[HX1,HY1]], H )

.

threeOutOfFive( Player, L , H, RX, RY ):-
    %TODO
    ( (RX = X1, RY = Y1) ; relativeTo(RX, RY, X1, Y1) ), 
    call( Player, X1, Y1),

    ( (RX = X2, RY = Y2) ; relativeTo(RX, RY, X2, Y2) ), 
    relativeTo( X1, Y1, X2, Y2),
    (X2 > X1 ; (X1 = X2, Y2 > Y1) ),
    call( Player, X2, Y2),
    directionType( N, X1, Y1, X2, Y2),

    ( (RX = X3, RY = Y3) ; relativeTo(RX, RY, X3, Y3) ), 
    relativeTo( X1, Y1, X3, Y3),
    (X3 > X2 ; (X2 = X3, Y3 > Y2)),
    call( Player, X3, Y3),
    directionType( N, X1, Y1, X3, Y3),

    relativeTo( X1, Y1, HX1, HY1 ),
    relativeTo( X3, Y3, HX1, HY1 ),
    directionType( N , X1, Y1, HX1, HY1),
    validForP( Player, HX1, HY1 ),

    relativeTo( HX1, HY1, HX2, HY2 ),
    relativeTo( X1, Y1, HX2, HY2 ),
    relativeTo( X3, Y3, HX2, HY2 ),
    (HX2 > HX1 ; (HX1 = HX2, HY2 > HY1) ),
    directionType( N , X1, Y1, HX2, HY2),
    validForP( Player, HX2, HY2 ),

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L ),
    list_to_set( [[HX1,HY1],[HX2,HY2]], H )
.

direction( 1, 1 ).
direction( 1, -1 ).
direction( 1, 0 ).
direction( 0, 1 ).

% type 3
live4( Player, L, H , RX, RY ):-
    direction( OX, OY ),
    between( 0, 3 , Offset),
    X1 is RX - Offset*OX, Y1 is RY - Offset*OY,

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),

    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( Player, HX1, HY1),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( Player, HX2, HY2),
    
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    list_to_set( [[HX1,HY1],[HX2,HY2]], H )
.

jump3( Player, L, H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( Player, HX1, HY1 ),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( Player, HX2, HY2 ),
    ( 
        (call( Player, X2, Y2 ), validForP( Player, X3, Y3), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X3,Y3],[HX2,HY2]], H ));
        (call( Player, X3, Y3 ), validForP( Player, X2, Y2), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X2,Y2],[HX2,HY2]], H ))
    )
.
straight3( Player, L, H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( Player, HX1, HY1 ),
    HX2 is X3+OX, HY2 is Y3+OY,
    validForP( Player, HX2, HY2 ),
    (　( HX3 is HX1-OX, HY3 is HY1-OY )　; ( HX3 is HX2+OX, HY3 is HY2+OY ) ),
    validForP( Player, HX3, HY3 ),

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L ),
    list_to_set( [[HX1,HY1],[HX2,HY2],[HX3,HY3]], H )
.

live3( Player, L, H ):- straight3( Player, L, H ) .
live3( Player, L, H ):- jump3( Player, L, H ) .

longLine( Player, L ):-
    direction( OX, OY ),

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    X6 is X5+OX, Y6 is Y5+OY,
    call( Player, X6, Y6),

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5],[X6,Y6]], L )
.

% black only
doubleLive4( Player, X, Y ):-
    addMove( Player, X, Y),
    ( fourOutOfFive( Player, L1, _), fourOutOfFive(Player, L2,_), L1 \= L2, member([X,Y], L1), member([X,Y], L2) )
->  undoMove( Player, X, Y)
;   undoMove( Player, X, Y), false
.
doubleLive3( Player, X, Y ):-
    addMove( Player, X, Y),
    ( live3( Player, L1, _), live3(Player, L2, _), L1 \= L2 , member([X,Y],L1), member([X,Y],L2) )
->  undoMove( Player, X, Y)
;   undoMove( Player, X, Y), false
.
triggerLongLine( Player, X, Y ):-
    addMove( Player, X, Y),
    ( longLine( Player, L1), member([X,Y], L1) )
->  undoMove( Player, X, Y)
;   undoMove( Player, X, Y), false
.


validForP( white, X, Y):- isEmpty( X, Y ), isValid(X,Y) .
validForP( black, X, Y ):-
    % ! TODO !
    isEmpty( X, Y ), isValid(X,Y)
    %\+doubleLive3( black, X, Y)
   %\+doubleLive4( black, X, Y),
   %\+triggerLongLine( black, X, Y)
.

winSecure( Player ):-
    %fourOutOfFive( Player, _, H), aggregate_all(count, member([_,_],H), Result), Result > 1
    live4( Player, _, _) ; ( fourOutOfFive( Player, _, H), member([X,Y],H), opponent( Player, OtherP), \+validForP( OtherP, X, Y) )
.

determineSide( white ):-
    aggregate_all(count, white(_,_), W),
    aggregate_all(count, black(_,_), B),
    B>W
.
determineSide( black ):- \+determineSide( white ).

opponent( white, black ).
opponent( black, white ).

main :-
    read_predicates(_),
    determineSide( Player ),
    opponent( Player, OtherP ),
    % if player got 4/5 -> go for win
    (
        fourOutOfFive( Player, L, H ), member([X,Y],H)
    ->  writeMove( Player, X, Y),halt
    ;   true
    ),
    % enumarate all relative point
    forall( fourOutOfFive( OtherP, _ ,H ), ( member([X,Y],H), addMove( try, X, Y ))),
    forall( (call(OtherP,X,Y), threeOutOfFive( OtherP, _ ,H ,X, Y ), member([TX,TY],H)), ( try(TX,TY)->true;addMove( try, TX, TY ) ) ),
    forall( (call(Player,X,Y), threeOutOfFive( Player, _ ,H ,X, Y ), member([TX,TY],H)), ( try(TX,TY)->true;addMove( try, TX, TY ) ) ),
    forall( 
        %( try(X,Y), validForP( Player, X, Y) ),
        try(X,Y), 
        (
            writeMove( try, X, Y),
            addMove( Player, X, Y ),
            (
                fourOutOfFive( OtherP, _, _) 
            ->  (   undoMove( Player, X, Y)  )
            ;   ( 
                    (threeOutOfFive( OtherP, _, _,0,0), \+fourOutOfFive( Player, _, _))
                    ->  (
                            forall( ( threeOutOfFive( OtherP, L ,H ,0,0), member([X1,Y1],H) ), (try(X1,Y1)->true;addMove( try2, X1, Y1 ) ) ),
                            forall( try2(X1,Y1), 
                                    (
                                        writeMove( try2, X1, Y1),
                                        addMove( OtherP, X1, Y1 ),
                                        (
                                            winSecure( OtherP )
                                        ->  undoMove( OtherP, X1, Y1), false
                                        ;   true
                                        ),undoMove( OtherP, X1, Y1)
                                    )
                            )
                            ->  retractall(try2(_,_)), undoMove( Player, X, Y), writeMove( Player, X, Y )%, halt
                            ;   retractall(try2(_,_)), undoMove( Player, X, Y)
                        )
                )
            ;   undoMove( Player, X, Y), writeMove( Player, X, Y ) %,halt
            )
        ) 
    ),
    writeMove( Player, 1, 1) %,halt
.