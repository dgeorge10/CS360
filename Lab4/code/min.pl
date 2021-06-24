min(X,[X]).
min(X,[Y,Z|Rest]) :- Y =< Z, min(X,[Y|Rest]).
min(X,[Y,Z|Rest]) :- Y > Z, min(X,[Z|Rest]).
