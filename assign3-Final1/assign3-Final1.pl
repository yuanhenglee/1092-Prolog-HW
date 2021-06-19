:- initialization main, halt.
:- dynamic white/2.
:- dynamic black/2.
:- table relativeTo/4.
:- dynamic cached_V/2.
:- dynamic cached_NV/2.
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
    call( Player, X, Y);
    forall( 
        relativeTo(X,Y,XR,YR),
        (
            (cached_V(XR,YR) -> retract(cached_V(XR,YR));true),
            (cached_NV(XR,YR) -> retract(cached_NV(XR,YR));true)
        )
    ),
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    assert(Term)
.

undoMove( Player, X, Y ):-
    \+call( Player, X, Y);
    forall( 
        relativeTo(X,Y,XR,YR),
        (
            (cached_V(XR,YR) -> retract(cached_V(XR,YR));true),
            (cached_NV(XR,YR) -> retract(cached_NV(XR,YR));true)
        )
    ),
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


% 5 types of just4:
% 1
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    validForP( white, X5, Y5),
    %\+triggerLongLine( Player , X5, Y5 ),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    list_to_set( [[X5,Y5]], H )
.
% 2
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    validForP( white, X4, Y4),
    %\+triggerLongLine( Player , X4, Y4 ),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X5,Y5]], L ),
    list_to_set( [[X4,Y4]], H )
.
% 3
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    validForP( white, X3, Y3),
    %\+triggerLongLine( Player , X3, Y3 ),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X3,Y3]], H )
.
% 4
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    validForP( white, X2, Y2),
    %\+triggerLongLine( Player , X2, Y2 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X2,Y2]], H )
.
% 5
just4( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X2, Y2),
    X1 is X2-OX, Y1 is Y2-OY,
    validForP( white, X1, Y1),
    %\+triggerLongLine( Player , X1, Y1 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    list_to_set( [[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X1,Y1]], H )
.

%1
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L ),

    HX2 is X1-OX, HY2 is Y1-OY, 
    HX1 is HX2-OX, HY1 is HY2-OY,
    HX3 is X3+OX, HY3 is Y3+OY, 
    HX4 is HX3+OX, HY4 is HY3+OY,

    (
        (validForP( white, HX1, HY1), validForP( white, HX2, HY2), list_to_set( [[HX1,HY1],[HX2,HY2]], H ) );
        (validForP( white, HX2, HY2), validForP( white, HX3, HY3), list_to_set( [[HX2,HY2],[HX3,HY3]], H ) );
        (validForP( white, HX3, HY3), validForP( white, HX4, HY4), list_to_set( [[HX3,HY3],[HX4,HY4]], H ) )
    )
.
% 2
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    validForP( white, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5 ),
    ( 
        (validForP( white, X4, Y4 ), call( Player, X2, Y2 ), list_to_set( [[X1,Y1],[X2,Y2],[X5,Y5]], L ), list_to_set( [[X4,Y4],[X3,Y3]], H ));
        (validForP( white, X2, Y2 ), call( Player, X4, Y4 ), list_to_set( [[X1,Y1],[X5,Y5],[X4,Y4]], L ), list_to_set( [[X3,Y3],[X2,Y2]], H ))
    )
.
% 3
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    HX2 is X4+OX, HY2 is Y4+OY,
    ( 
        (validForP( white, HX1, HY1 ), call( Player, X2, Y2 ), validForP( white, X3, Y3), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X3,Y3]], H ));
        (validForP( white, HX1, HY1 ), call( Player, X3, Y3 ), validForP( white, X2, Y2), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X2,Y2]], H ));
        (validForP( white, HX2, HY2 ), call( Player, X2, Y2 ), validForP( white, X3, Y3), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[X3,Y3],[HX2,HY2]], H ));
        (validForP( white, HX2, HY2 ), call( Player, X3, Y3 ), validForP( white, X2, Y2), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[X2,Y2],[HX2,HY2]], H ))
    )
.
% 4
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),

    validForP( white, X2, Y2),
    validForP( white, X4, Y4),

    list_to_set( [[X1,Y1],[X3,Y3],[X5,Y5]], L ),
    list_to_set( [[X2,Y2],[X4,Y4]], H )
.

direction( 1, 1 ).
direction( 1, -1 ).
direction( 1, 0 ).
direction( 0, 1 ).

live4( Player, L, H ):-
    direction( OX, OY ),

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),

    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( Player , HX1, HY1 ),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( Player , HX2, HY2 ),
    
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
    validForP( white, HX1, HY1 ),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( white, HX2, HY2 ),
    ( 
        (call( Player, X2, Y2 ), validForP( white, X3, Y3), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[X3,Y3]], H ));
        (call( Player, X3, Y3 ), validForP( white, X2, Y2), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[X2,Y2]], H ))
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
    validForP( white, HX1, HY1 ),
    HX2 is X3+OX, HY2 is Y3+OY,
    validForP( white, HX2, HY2 ), 
    HX3 is HX1-OX, HY3 is HY1-OY ,
    HX4 is HX2+OX, HY4 is HY2+OY ,
    (　
        (validForP( white, HX3, HY3),list_to_set( [[HX1,HY1]], H ) );
        (validForP( white, HX4, HY4),list_to_set( [[HX2,HY2]], H ) )
    ),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L )
.

live3( Player, L, H ):- straight3( Player, L, H ) .
live3( Player, L, H ):- jump3( Player, L, H ) .

perfect5( Player, L ):-
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

    HX1 is X1-OX, HY1 is Y1-OY,
    \+call( Player, HX1, HY1),
    HX2 is X5+OX, HY2 is Y5+OY,
    \+call( Player, HX2, HY2),

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L )
.

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
    ( 
        just4( Player, L1, [[HX1,HY1]]), just4(Player, L2,[[HX2,HY2]]),
        L1 \= L2, member([X,Y], L1), member([X,Y], L2),
        validForP( Player, HX1, HY1), validForP( Player, HX2, HY2)
    )
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
triggerPerfect5( Player, X, Y ):-
    addMove( Player, X, Y),
    ( perfect5( Player, L1), member([X,Y], L1) )
->  undoMove( Player, X, Y)
;   undoMove( Player, X, Y), false
.


validForP( white, X, Y ):- isEmpty( X, Y ), isValid(X,Y) .
validForP( black, X, Y ):- cached_V(X,Y), \+cached_NV(X,Y).
validForP( black, X, Y ):-
    (cached_NV(X,Y) -> false;true),
    (cached_V(X,Y) -> true,! ;true),
    (
        (
            isEmpty( X, Y ), isValid(X,Y),
            (
                triggerPerfect5( black, X, Y );
                (
                    \+doubleLive3( black, X, Y),
                    \+doubleLive4( black, X, Y),
                    \+triggerLongLine( black, X, Y)
                )
            )
        )
    ->  assert( cached_V(X,Y) )
    ;   assert( cached_NV(X,Y) ),false
    )
.

winSecure( Player ):-
    %just4( Player, _, H), aggregate_all(count, member([_,_],H), Result), Result > 1
    live4( Player, _, _) ; ( just4( Player, _, H), member([X,Y],H), opponent( Player, OtherP), \+validForP( OtherP, X, Y) )
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
    % if player got 4/5 -> go for win ( 1 )
    ( ( just4( Player, _, H1 ), member([X1,Y1],H1), validForP( Player, X1,Y1) ) ->  (writeMove( Player, X1, Y1),halt) ; true ),
    % if other player got 4/5 -> defend ( 2 3 )
    forall( 
        ( just4( OtherP , _, H2 ), member([X2,Y2],H2), validForP( Player, X2, Y2 ) ),
        ( try(X2,Y2) ->  true ;   addMove(try, X2, Y2))
    ),
    forall( 
        ( try(X,Y) ),
        (
            %writeMove( try, X, Y),
            addMove( Player, X, Y ),
            (
                ( just4( OtherP, _, H3), member([X3,Y3], H3), validForP( OtherP, X3, Y3)  )
            ->  true
            ;   writeMove( Player, X, Y),halt 
            ),
            undoMove( Player, X, Y)
        )
    ),
    %( just4( OtherP, _, _) -> write("= = O"); true ),
    %( just4( Player, _, [[10,3]]) -> write("= = P"); true ),
    retractall( try(_,_) ),
    % if player got 3 -> make it live4 or doubleD4 or D4 with banned  ( 4 )
    forall(
        ( just3( Player, _, H4 ), member([X4,Y4],H4), validForP( Player, X4, Y4 )),
        (
            %write(X4), write(Y4),
            ( 
                ( doubleLive4( Player, X4, Y4)  );
                addMove( Player, X4, Y4),
                (
                    (   live4( Player, L4,_) , (X4 \= 5;Y4 \= 6)
                    );
                    ( just4( Player, _, H41 ), member([X41,Y41],H41), \+validForP( OtherP , X41, Y41), validForP( Player, X41, Y41) )
                )
            )
        ->  ( undoMove( Player, X4, Y4 ), writeMove( Player, X4, Y4),halt )
        ;   undoMove( Player, X4, Y4 )
        )
    ),

    % strange fix??
    (
        (   jump3( OtherP, _, [[X41,Y41]]), validForP( black, X41, Y41)   )
    ->  writeMove( Player, X41, Y41),halt 
    ;   true
    ),

    % if other player got 3 -> make sure not to lose 
    forall(
        ( just3( OtherP, L5, H5 ), member([X5,Y5],H5), validForP( Player, X5, Y5 )  ),
        (
            %riteMove( Player, X5, Y5),
            addMove( Player, X5, Y5),
            (
                forall(
                    ( just3( OtherP, _, H52 ), member([X52,Y52],H52), validForP( OtherP, X52, Y52 ) ),
                    (
                        \+doubleLive4( OtherP, X52, Y52),
                        %writeMove( OtherP, X52, Y52),
                        addMove( OtherP, X52, Y52),
                        \+live4( OtherP, _,_)->( undoMove( OtherP, X52, Y52 ) );( undoMove( OtherP, X52,Y52), false )
                        %( just4( OtherP, _, H53 ), member([X53,Y53],H53), \+validForP( Player , X53, Y53), validForP( OtherP ,X53, Y53))
                    )
                )
            ->  ( undoMove( Player, X5, Y5 ), writeMove( Player, X5, Y5),halt )
            ;   undoMove( Player, X5, Y5 )
            )
        )
    ),
    %write("= ="),
    writeMove( Player, 1, 1) %,halt
.