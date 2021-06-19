:- initialization main, halt.

read_lines([H|T]) :-
  read_line_to_codes(user_input, H), H \= end_of_file, read_lines(T).
  %writeln(H),
  %writeln(T).
read_lines([]).

write_lines([]).
write_lines([H|T]) :-
  writef("%s\n", [H]), write_lines(T).

even(N):- N mod 2 =:= 0.

odd(N):- N mod 2 =:= 1.


threeNPlusOne(1,1).
threeNPlusOne(N,M) :-
    even(N),
    %M1 is M - 1,
    N1 is N/2,
    threeNPlusOne(N1, M1),
    M is M1 + 1.
    %N is N1 * 2,
threeNPlusOne(N,M) :-
    odd(N),
    %M1 is M - 1,
    N1 is (3 * N) + 1,
    threeNPlusOne(N1, M1),
    M is M1 + 1.
    %N is (N1-1)/3,

process([]).
process([H|T]):-
    number_codes(N, H),
    threeNPlusOne(N, X),
    writeln(X),
    process(T).

main :-
  read_lines(X),
  %write_lines(X),
  process(X).

