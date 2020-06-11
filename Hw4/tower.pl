% Homework 4: Towers Solver
% Grade Received: 94.4%
% If you found this file helpful, please consider reaching out to me:
%   Website: faithtwardzik.com
%   Instagram: @faithtwardzik

%%%%%
% plain_tower implementation
%%%%%

% reverses all lists in matrix X
reverse_all(X, RX) :-
    maplist(reverse, X, RX).

% checks if els in row are unique
row_unique(Row) :-
    sort(Row, Sorted),
    length(Row, N_elements),
    length(Sorted, N_unique_elements),
    N_elements == N_unique_elements.

% transposes a matrix so columns are now rows & vice versa
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% checks if there are N lists in T, each list has distinct elements
% will be applied on rows and columns (rows of transposed matrix) 
check_T(T, N) :-
    maplist(row_unique, T),
    length(T, N).

% Formerly used in plain_tower, but not fast enough, so not used in
% final implementation
matrix_checks(T, N) :-
    length(T, N),
    len_col(T, N),
    check_domain(T, N),
    maplist(row_unique, T),
 %  check_domain(T, N),
 %  check_T(T, N),
 %  len_col(T, N),
    transpose(T, T_col),
    maplist(row_unique, T_col).
 %  check_domain(T_col, N),
 %  check_T(T_col, N).
    
% finds all permutations of nums within domain,
% we will check to see if all our T lists map to a permutation
within_domain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% make sure num of elements in T lists are from 1-N
check_domain([],_).
check_domain([R_head | R_tail], N) :-
    within_domain(N, Domain), 
    permutation(Domain, R_head), 
    check_domain(R_tail, N).

% takes list and returns SI with only the strictly increasing els
strictly_inc([Hd|Tl], SI) :-
    strictly_inc(Tl, Hd, [Hd], SI).
strictly_inc([], _, Sublist, Sublist). 
strictly_inc([Hd|Tl], Maxval, TmpSublist, SI) :-
    Hd >= Maxval,
    append(TmpSublist, [Hd], NextSublist),
    strictly_inc(Tl, Hd, NextSublist, SI).
strictly_inc([Hd|Tl], Maxval, TmpSublist, SI) :-
    Hd < Maxval,
    strictly_inc(Tl, Maxval, TmpSublist, SI).

% applies strictly_inc to a list of lists
all_inc([], []).
all_inc([R_head | R_tail], [Count_head | Count_tail]) :-
    strictly_inc(R_head, Count_head_list),
    length(Count_head_list, Count_head),
    all_inc(R_tail, Count_tail).

% utilizes generate_permutation to build array and prune search,
% much quicker solution than using matrix_checks
plain_tower(N, T, C) :-
    counts(A, B, L, R) = C,
    length(A, N),
    length(B, N),
    length(L, N),
    length(R, N),

    within_domain(N, Domain),
    generate_permutation(Domain, T, N, L, R),
    transpose(T, T_tr),
    maplist(row_unique, T_tr),

    % Above row of array
    all_inc(T_tr, A),

    % Below row of array
    reverse_all(T_tr, Tr_Rev),
    all_inc(Tr_Rev, B).

generate_permutation(Domain, [Row], 1, [L], [R]):-
    permutation(Domain, Row),
    strictly_inc(Row, W),
    length(W, L),
    reverse(Row, Row_Rev),
    strictly_inc(Row_Rev, E),
    length(E, R).

generate_permutation(Domain, [Row|Rest], K, [L|L_Rest], [R|R_Rest]):-
    K > 1,
    K1 is K-1,

    permutation(Domain, Row),
    strictly_inc(Row, W),
    length(W, L),
    reverse(Row, Row_Rev),
    strictly_inc(Row_Rev, E),
    length(E, R),

    generate_permutation(Domain, Rest, K1, L_Rest, R_Rest).

%%%%%
% tower implementation
%%%%%

% checks domain using finite domain solver
check_domain_fd([], _).
check_domain_fd([R_head | R_tail], N) :-
    fd_domain(R_head, 1, N),
    check_domain_fd(R_tail, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

tower(N, T, C) :-
    % check length, uniqueness, domain, then transpose
    length(T, N),
    len_col(T, N),
    check_domain_fd(T, N),
    maplist(fd_all_different, T),
    
    transpose(T, T_tr),
    maplist(fd_all_different, T_tr),
    maplist(fd_labeling, T),

    counts(A, B, L, R) = C,
    length(A, N),
    length(B, N),
    length(L, N),
    length(R, N),
  
    % Above row of array
    all_inc(T_tr, A),

    % Below row of array
    reverse_all(T_tr, Tr_Rev),
    all_inc(Tr_Rev, B),

    % Left row of array
    all_inc(T, L),

    % Right row of array
    reverse_all(T, T_Rev),
    all_inc(T_Rev, R).

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.

tower_time(T1) :-
    statistics(cpu_time, [B|_]),
    tower(5, T, counts([1,4,3,2,2],[4,1,2,2,2],[1,4,2,3,2],[3,1,2,2,2])),
    statistics(cpu_time, [E|_]),
    T1 is E - B.

plain_time(T2) :-
    statistics(cpu_time, [B1|_]),
    plain_tower(5, T, counts([1,4,3,2,2],[4,1,2,2,2],[1,4,2,3,2],[3,1,2,2,2])),
    statistics(cpu_time, [E1|_]),
    T2 is E1 - B1.

speedup(R) :-
    tower_time(Time1),
    plain_time(Time2),
    R is Time2 / Time1.









%%%
% NOTE: For some of the above predicates, I used parts of the TA
% hint code provided for us or adapted it to work with my own code 
%%%










