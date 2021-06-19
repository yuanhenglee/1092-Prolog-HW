:- initialization main, halt.
:- dynamic say/3.
:- dynamic say/2.
:- dynamic divine/1.
:- dynamic evil/1.
:- dynamic human/1.
:- dynamic day/0.
:- dynamic night/0.

read_predicates([H|T]) :-
    read_line_to_codes(user_input, H), 
    string_codes(Input, H),
    (   Input = "-"
    ->  process,
        read_queries(T)
    ;   term_string(Term, Input),
        assert(Term), % 將 Term assert 進 KB
        read_predicates(T)).
read_predicates([]).

read_queries([H|T]) :-
    read_line_to_codes(user_input, H),
    H \= end_of_file,
    string_codes(Input, H),
    term_string(Query, Input),
    query(Query),
    read_queries(T).
read_queries([]).

query(Q) :- 
    (Q -> writeln(yes) ; writeln(no)).

process:-
    % enumerate all possible Species
    forall((member(A, [divine, evil, human]), member(B, [divine, evil, human]), member(C, [divine, evil, human]), member(D, [divine, evil, human]), member(E, [divine, evil, human]), member(Day, [day, night])),
        (   Creatures = [a, b, c, d, e], Species = [A, B, C, D, E],
            test(a, A, Day, Creatures, Species), test(b, B, Day, Creatures, Species), 
            test(c, C, Day, Creatures, Species), test(d, D, Day, Creatures, Species), 
            test(e, E, Day, Creatures, Species) 
        
        ->  insert(Creatures, Species), assert(Day)
        %    ,writeln(Creatures),writeln(Species),writeln(Day)
        ;   true    )
    ),
    % remove answer can't be specify
    ((day,night)    
    ->  (retract(day),retract(night));true),
    forall(member(Target,[a,b,c,d,e]),removeUndetermined(Target))
.

removeUndetermined(Target):-
    (divine(Target), \+evil(Target), \+human(Target));
    (\+divine(Target), evil(Target), \+human(Target));
    (\+divine(Target), \+evil(Target), human(Target));
    (   
        forall(divine(Target), retract(divine(Target))),
        forall(evil(Target), retract(evil(Target))),
        forall(human(Target), retract(human(Target)))
        %,write("undetermined: "),writeln(Target)
    )  
.

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

findSpeciesInList(Target,CreatureList, SpeciesList):-
    indexOf(CreatureList, Target, Index)
.

speciesBehavior(divine, \+lying, _).
speciesBehavior(evil, lying, _).
speciesBehavior(human, \+lying, day).
speciesBehavior(human, lying, night).

test(Creature, Species, Day, CreatureList, SpeciesList) :-
    (
        speciesBehavior( Species, lying, Day )
        *->(     % lying case
            (forall(
                    (member(BS, [divine, evil, human]), say(Creature, B , BS_fromA))
                    ,(nth0(BI,CreatureList,B),nth0(BI,SpeciesList,BS_fromPred),BS_fromPred\=BS_fromA)
                ),
            forall(
                    (member(BS_fromA, [\+divine, \+evil, \+human]), say(Creature, B , BS_fromA))
                    ,(nth0(BI,CreatureList,B),nth0(BI,SpeciesList,BS),(\+BS) = BS_fromA)
                ),
            forall(
                    (member(BL, [lying,\+lying]), say(Creature, B , BL))
                    ,(nth0(BI,CreatureList,B), nth0(BI,SpeciesList,BS_fromPred), \+speciesBehavior(BS_fromPred, BL, Day))%, speciesBehavior(BS,\+BL,Day),)
                ),
            forall(
                    (member(DN, [day,night]), say(Creature, DN))
                    ,(DN\=Day)
                )
            )
        )      
        ;(      % not lying case
            (forall(
                    (member(BS, [divine, evil, human]), say(Creature, B , BS))
                    ,(nth0(BI,CreatureList,B),nth0(BI,SpeciesList,BS))
                ),
            forall(
                    (member(BS_fromA, [\+divine, \+evil, \+human]), say(Creature, B , BS_fromA))
                    ,(nth0(BI,CreatureList,B),nth0(BI,SpeciesList,BS),(\+BS) \= BS_fromA)
                ),
            forall(
                    (member(BL, [lying,\+lying]), say(Creature, B , BL))
                    ,(nth0(BI,CreatureList,B), nth0(BI,SpeciesList,BS_fromA), speciesBehavior(BS_fromA, BL, Day))
                    %,(nth0(BI,CreatureList,B), nth0(BI,SpeciesList,BS_fromPred), BS_fromPred=BS_fromA)%, speciesBehavior(BS,\+BL,Day),)
                ),
            forall(
                    (member(DN, [day,night]), say(Creature, DN))
                    ,(DN=Day)
                )
            )
        )
    )
    %->(write(Creature), writef(" works under "), write(SpeciesList),writeln(Day))
    %;(write(Creature), writef(" fails under "), write(SpeciesList),writeln(Day)),false
    .  

    %;   Species=:='divine' -> true 
%   這裡還需要寫

insert([], []).
insert([HofC|TofC], [HofS|TofS]) :-
    atomic_list_concat([HofS, "(", HofC, ")"], Fact),
    term_to_atom(Term, Fact),
    assert(Term),
    insert(TofC, TofS).


main :-
    read_predicates(X).