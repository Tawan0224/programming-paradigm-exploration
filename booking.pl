% Simple Booking System (Prolog)
% slot(Index, Value).
% Value is free or Name (an atom)
%
% ?- [booking].
% ?- init_slots(5).
% ?- show_slots.
% ?- book(2, 'alice').
% ?- cancel(2).
% ?- book(0, bob).
% ?- show_slots.
%
% Commands implemented:
%   init_slots(N).   initialize N slots
%   show_slots.      prints all slots
%   book(I, Name).   book slot I with Name
%   cancel(I).       cancel booking at I
%

:- dynamic slot/2.

%% init_slots(N) - initialize N slots to free (indices 0..N-1)
init_slots(N) :-
    integer(N), N > 0,
    retractall(slot(_, _)),
    init_slots_loop(0, N).

init_slots_loop(I, N) :-
    ( I < N ->
        assertz(slot(I, free)),
        I1 is I + 1,
        init_slots_loop(I1, N)
    ; true ).

%% show_slots 
show_slots :-
    findall(I-Value, slot(I, Value), Pairs),
    sort(Pairs, Sorted),
    writeln('Slots:'),
    forall(member(I-V, Sorted),
           ( format('  [~w] ', [I]),
             (V = free -> writeln('FREE') ; format('BOOKED by ~w~n', [V])) )).

%% book(Index, Name) 
book(I, Name) :-
    integer(I),
    slot(I, free),
    !,
    retract(slot(I, free)),
    assertz(slot(I, Name)),
    format('Booked slot ~w by ~w.~n', [I, Name]).
book(I, _Name) :-
    ( slot(I, _) -> writeln('Slot already booked.') ; writeln('Index out of range.'), fail ).

%% cancel(Index) 
cancel(I) :-
    integer(I),
    slot(I, Val),
    Val \= free,
    !,
    retract(slot(I, Val)),
    assertz(slot(I, free)),
    format('Canceled slot ~w (was booked by ~w).~n', [I, Val]).
cancel(I) :-
    ( slot(I, free) -> writeln('Slot already free.'), fail ; writeln('Index out of range.'), fail ).

%% book_cmd and cancel_cmd to parse strings from user 
book_cmd(Str) :-
    split_string(Str, " ", "", [Istr|Rest]),
    number_string(I, Istr),
    atomic_list_concat(Rest, ' ', NameStr),
    atom_string(Name, NameStr),
    ( book(I, Name) -> true ; true ).

cancel_cmd(Str) :-
    split_string(Str, " ", "", [Istr|_]),
    number_string(I, Istr),
    ( cancel(I) -> true ; true ).
