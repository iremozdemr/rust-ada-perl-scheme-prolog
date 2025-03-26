eval(X + Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val + Y_val.
eval(X - Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val - Y_val.
eval(X * Y, R) :- eval(X, X_val), eval(Y, Y_val), R is X_val * Y_val.
eval(X / Y, R) :- eval(X, X_val), eval(Y, Y_val), Y_val \= 0, R is X_val / Y_val.
eval(N, N) :- number(N).

process_input(Input, Result) :-
    term_string(Term, Input), 
    eval(Term, Result).

calculator :-
    write('Hesaplamak istediğiniz ifadeyi girin: '),
    read_line_to_string(user_input, Input),
    process_input(Input, Result),
    write('Sonuç: '), write(Result), nl.

% Örnek kullanım
% ?- calculator.
% Hesaplamak istediğiniz ifadeyi girin: 2+3.
% Sonuç: 5
% true.

% ?- calculator.
% Hesaplamak istediğiniz ifadeyi girin: 10*5.
% Sonuç: 50
% true.

% ?- calculator.
% Hesaplamak istediğiniz ifadeyi girin: 10/2.
% Sonuç: 5.0
% true.