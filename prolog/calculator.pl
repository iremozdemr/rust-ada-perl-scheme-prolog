eval(X + Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val + Y_val.
eval(X - Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val - Y_val.
eval(X * Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val * Y_val.
eval(X / Y, R) :- eval(X, X_val), eval(Y, Y_val), Y_val \= 0, R is X_val / Y_val.
eval(N, N) :- number(N).

process_input(Input, Result) :-
    term_string(Term, Input), 
    eval(Term, Result).

calculator :-
    write('please enter the expression: '),
    read_line_to_string(user_input, Input),
    process_input(Input, Result),
    write('result: '), write(Result), nl.