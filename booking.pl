% Simple Booking System for Beginners
% How to use:
% 1. Start SWI-Prolog and load this file: ?- [booking].
% 2. Create slots: ?- make(3).
% 3. Show slots: ?- show.
% 4. Book slot: ?- book(0, alice).
% 5. Free slot: ?- free(0).

:- dynamic slot/2.

% Create N empty slots (0, 1, 2, ...)
make(N) :-
    retractall(slot(_, _)),    % Clear old slots
    make_slots(0, N),          % Create new slots
    format('Made ~w slots~n', [N]).

% Helper to create slots from 0 to N-1
make_slots(I, N) :-
    I < N,
    !,
    assertz(slot(I, empty)),   % Create empty slot
    I1 is I + 1,
    make_slots(I1, N).
make_slots(_, _).              % Stop when I >= N

% Show all slots in order
show :-
    findall(I, slot(I, _), SlotNumbers),     % Get all slot numbers
    sort(SlotNumbers, Sorted),               % Sort just the numbers
    forall(member(I, Sorted),                % For each slot in order
           (slot(I, Status),                 % Get its status
            format('~w: ', [I]),
            (Status = empty -> 
                writeln('EMPTY') ; 
                writeln(Status)))).

% Book a slot with a name
book(SlotNum, Name) :-
    slot(SlotNum, empty),      % Check if slot is empty
    !,
    retract(slot(SlotNum, empty)),     % Remove empty status
    assertz(slot(SlotNum, Name)),      % Add name
    format('Slot ~w booked for ~w~n', [SlotNum, Name]).

book(SlotNum, _) :-
    slot(SlotNum, Name),       % Slot exists but not empty
    Name \= empty,
    !,
    format('Slot ~w already taken by ~w~n', [SlotNum, Name]).

book(SlotNum, _) :-            % Slot doesn't exist
    format('Slot ~w does not exist~n', [SlotNum]).

% Free a slot
free(SlotNum) :-
    slot(SlotNum, Name),       % Check if slot exists
    Name \= empty,             % And is not already empty
    !,
    retract(slot(SlotNum, Name)),      % Remove name
    assertz(slot(SlotNum, empty)),     % Make empty
    format('Slot ~w is now free~n', [SlotNum]).

free(SlotNum) :-
    slot(SlotNum, empty),      % Already empty
    !,
    format('Slot ~w is already empty~n', [SlotNum]).

free(SlotNum) :-               % Doesn't exist
    format('Slot ~w does not exist~n', [SlotNum]).