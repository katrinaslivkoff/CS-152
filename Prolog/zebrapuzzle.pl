% CS 152 The Zebra Puzzle

% working_directory(CWD, '/Users/katsl/Documents/CS 152/Prolog').

% Helper predicates

right(X, Y, [X, Y|_]).
right(X, Y, [_|T]) :- right(X, Y, T).

next(X, Y, L):- right(X, Y, L).
next(X, Y, L):- right(Y, X, L).

middle(X, [_, _, X, _, _]).
first(X, [X|_]).
fiveHouses([_, _, _, _, _]).


% Answer the question: Who owns the zebra and who drinks water?

solution(ZebraOwner, WaterDrinker) :-
fiveHouses(MainStreet),
% Who owns the zebra?
member(house(ZebraOwner, _, zebra, _,  _), MainStreet),
% Who drinks water?
member(house(WaterDrinker, _, _, water, _), MainStreet),
member(house(teacher, red, _, _, _), MainStreet), % The teacher lives in the red house
member(house(dentist, _, dog, _, _), MainStreet), % The dentist owns a dog
member(house(_, green, _, coffee, _), MainStreet), % In the green house they drink coffee
member(house(programmer, _, _, tea, _), MainStreet), % The programmer drinks tea
% The green house is immediately to the right of the purple house
right(house(_, purple, _, _, _), house(_, green, _, _, _), MainStreet),
member(house(_, _, llama, _, honda), MainStreet), % The Honda driver has a llama
member(house(_, yellow, _, _, ford), MainStreet), % The Ford driver lives in the yellow house
middle(house(_, _, _, milk, _), MainStreet), % The person in the middle house drinks milk
first(house(carpenter, _, _, _, _), MainStreet),% The carpenter lives in first house on left
% The VW driver lives next to the person who has a cat
next(house( _, _, _, _, vw), house(_, _, cat, _, _), MainStreet),
% The Ford driver lives in the house next to the person who owns a horse
next(house(_, _, _, _, ford),  house(_, _, horse, _, _), MainStreet),
member(house(_, _, _, oj, toyota), MainStreet), % The Toyota driver drinks orange juice
member(house(engineer, _, _, _, buick), MainStreet), % The engineer drives a Buick
% The carpenter lives next to the blue house
next(house( _, blue, _, _, _), house(carpenter, _, _, _, _), MainStreet),
% The VW driver lives next to the person who drinks water
next(house( _, _, _, _, vw), house(_, _, _, water, _), MainStreet).

solveAll(MainStreet) :-fiveHouses(MainStreet),
member(house(teacher, red, _, _, _), MainStreet), % The teacher lives in the red house
member(house(dentist, _, dog, _, _), MainStreet), % The dentist owns a dog
member(house(_, green, _, coffee, _), MainStreet), % In the green house they drink coffee
member(house(programmer, _, _, tea, _), MainStreet), % The programmer drinks tea
% The green house is immediately to the right of the purple house
right(house(_, purple, _, _, _), house(_, green, _, _, _), MainStreet),
member(house(_, _, llama, _, honda), MainStreet), % The Honda driver has a llama
member(house(_, yellow, _, _, ford), MainStreet), % The Ford driver lives in the yellow house
middle(house(_, _, _, milk, _), MainStreet), % The person in the middle house drinks milk
first(house(carpenter, _, _, _, _), MainStreet),% The carpenter lives in first house on left
% The VW driver lives next to the person who has a cat
next(house( _, _, _, _, vw), house(_, _, cat, _, _), MainStreet),
% The Ford driver lives in the house next to the person who owns a horse
next(house(_, _, _, _, ford),  house(_, _, horse, _, _), MainStreet),
member(house(_, _, _, oj, toyota), MainStreet), % The Toyota driver drinks orange juice
member(house(engineer, _, _, _, buick), MainStreet), % The engineer drives a Buick
% The carpenter lives next to the blue house
next(house( _, blue, _, _, _), house(carpenter, _, _, _, _), MainStreet),
% The VW driver lives next to the person who drinks water
next(house( _, _, _, _, vw), house(_, _, _, water, _), MainStreet).
