% CS 152 - Smart Spartans

% Helper predicates

ahead([X|T], X, Y):- member(Y, T).
ahead([_|T], X, Y):- ahead(T, X, Y).

third(X, [_, _,X, _, _]).
notfirst(X, [_|T]):- member(X, T).

first(X, [X|_]).
second(X, [_, X|_]).

fivePlaces([_, _, _, _, _]).

% Which colleges came in first and second place?

winners(First, Second) :-
fivePlaces(Place),
% Which college came in first?
first(team(First, _, _, _, _), Place),
% Which college came in second?
second(team(Second, _, _, _, _), Place),
member(team(deanza, _, _, _, _), Place),
member(team(_, steven, _, _, _), Place),
member(team(sjsu, tammy, _, _, _), Place), % 1) Tammy goes to SJSU.
member(team(_, alex, java, _, _), Place), % 2) Alex will only program in Java.
member(team(_, justin, _, amazon, _), Place), % 3) Justins team is sponsored by Amazon.
member(team(sjsu, _, _, facebook, _), Place), % 4) The SJSU team is sponsored by Facebook.
member(team(foothill, _, _, ibm, _), Place), % 5) The Foothill team is sponsored by IBM.
member(team(_, nicole, _, google, _), Place), % 6) Nicoles team is sponsored by Google.
member(team(foothill, _, javascript, _, _), Place), % 7) The Foothill team programs in JavaScript.
member(team(berkeley, _, _, _, orange), Place), % 8) The Berkeley team is wearing orange.
member(team(_, _, _, apple, red), Place), % 9) The team sponsored by Apple is wearing red.
member(team(ucsantacruz, _, _, _, purple), Place), % 10) The UC Santa Cruz team is wearing purple.
member(team(sjsu, _, _, _, blue), Place), % 11) The SJSU team is wearing blue.
ahead(Place, team(_, _, _, _, blue), team(_, _, _, _, purple)), % 1) The team wearing blue placed ahead of the team wearing purple.
ahead(Place, team(_, _, _, _, orange), team(_, _, haskell, _, _)), % 2) The team wearing orange placed ahead of the team programming in Haskell.
third(team(_, _, _, ibm, _), Place), % 3) The team sponsored by IBM came in third.
ahead(Place, team(_, _, python, _, _), team(_, _, _, _, blue)), % 4) The team programming in Python placed ahead of the team wearing blue.
notfirst(team(_, _, _, _, red), Place), % 5) The team wearing red did not place first.
ahead(Place, team(_, _, _, google, _), team(_, _, _, _, blue)), % 6) The team sponsored by Google placed ahead of the team wearing blue.
ahead(Place, team(_, _, haskell, _, _), team(_, _, javascript, _, _)), % 7) The team programming in Haskell placed ahead of the team programming in JavaScript.
last(Place, team(_, _, cplusplus, _, _)). % 8) The last team used C++.


% What are the detailed results of the hackathon?

solution(Place):- fivePlaces(Place),
member(team(deanza, _, _, _, _), Place),
member(team(_, steven, _, _, _), Place),
member(team(sjsu, tammy, _, _, _), Place), % 1) Tammy goes to SJSU.
member(team(_, alex, java, _, _), Place), % 2) Alex will only program in Java.
member(team(_, justin, _, amazon, _), Place), % 3) Justins team is sponsored by Amazon.
member(team(sjsu, _, _, facebook, _), Place), % 4) The SJSU team is sponsored by Facebook.
member(team(foothill, _, _, ibm, _), Place), % 5) The Foothill team is sponsored by IBM.
member(team(_, nicole, _, google, _), Place), % 6) Nicoles team is sponsored by Google.
member(team(foothill, _, javascript, _, _), Place), % 7) The Foothill team programs in JavaScript.
member(team(berkeley, _, _, _, orange), Place), % 8) The Berkeley team is wearing orange.
member(team(_, _, _, apple, red), Place), % 9) The team sponsored by Apple is wearing red.
member(team(ucsantacruz, _, _, _, purple), Place), % 10) The UC Santa Cruz team is wearing purple.
member(team(sjsu, _, _, _, blue), Place), % 11) The SJSU team is wearing blue.
ahead(Place, team(_, _, _, _, blue), team(_, _, _, _, purple)), % 1) The team wearing blue placed ahead of the team wearing purple.
ahead(Place, team(_, _, _, _, orange), team(_, _, haskell, _, _)), % 2) The team wearing orange placed ahead of the team programming in Haskell.
third(team(_, _, _, ibm, _), Place), % 3) The team sponsored by IBM came in third.
ahead(Place, team(_, _, python, _, _), team(_, _, _, _, blue)), % 4) The team programming in Python placed ahead of the team wearing blue.
notfirst(team(_, _, _, _, red), Place), % 5) The team wearing red did not place first.
ahead(Place, team(_, _, _, google, _), team(_, _, _, _, blue)), % 6) The team sponsored by Google placed ahead of the team wearing blue.
ahead(Place, team(_, _, haskell, _, _), team(_, _, javascript, _, _)), % 7) The team programming in Haskell placed ahead of the team programming in JavaScript.
last(Place, team(_, _, cplusplus, _, _)). % 8) The last team used C++.
