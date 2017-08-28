%%% Ensure the correct transpose/2 predicate is loaded
:- ensure_loaded(library(clpfd)).

%%% Reads in the files, solved the puzzle, and prints the solution
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).
%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FILE IN/OUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Read the contents of a file and save each line to a list
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).
%%% 

%%% Read all lines from a file and save them to a list, one line per element
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).
%%% 

%%% Read a line from a file and save it to a list, one letter per element
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).
%%% 

%%% Prints the finished puzzle to a file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).
%%% 

%%% Prints one row of the puzzle to a file
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).
%%% 

%%% Prints one character to a file
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).
%%% 

%%% Validates that the puzzle is a square
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).
%%% 

%%% Compares the length of two lists
samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).
%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PUZZLE SOLVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Takes a puzzle grid and a wordlist and solves the puzzle
solve_puzzle(Solution, [], Solution):- !.
solve_puzzle(Puzzle, Wordlist, Solved):-
	optimise_wordlist(Wordlist,Wordlist_Opt),
	fill_slots_H(Puzzle,Slots1,Puzzle1), 		% Fill out the puzzle with logical variables in every blank space (results = Puzzle1), also get all the horizontal word slots (results = Slots1) 
	flip(Puzzle1,0,Puzzle1_Flipped),			% Flip the puzzle for vertical use (results = Puzzle1_Flipped)
	fill_slots_V(Puzzle1_Flipped,Slots1,Slots),	% Take the vertical puzzle and append the slots for vertical words onto the end of the existing slot list SLOTS IS THE FINAL UNBOUND SLOT LIST
	flip(Puzzle1_Flipped,1,Puzzle_Final),		% Flip the puzzle back to normal
	!,											% Make these choices final
	insert_words(Wordlist_Opt,Slots),
	solve_puzzle(Puzzle_Final, [], Solved),!.	% Puzzle is now filled, return it as the solution
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WORD LIST OPTIMISATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Optimises the order of the word list to severely cut down 
%   on the amount of permutations needed to compute the final puzzle
optimise_wordlist(Wordlist,Wordlist_Optimised):- 
	qsort(Wordlist,Wordlist_Sorted),					% Sort the wordlist by length in descending order
	split_lengths(Wordlist_Sorted, Wordlist_Split),		% Split the wordlist into lists of same-length words
	qsort_a(Wordlist_Split, Wordlist_Split_Sorted),		% Sort the split wordlist by length in ascending order
	collapse(Wordlist_Split_Sorted,Wordlist_Optimised). % Remove one layer of nesting
%%%	

%%% Takes a sorted wordlist and separates all the words into lists of words the same length
split_lengths([W|Ws],Wordlist_Split):- 
	length(W,L),
	s_l([W|Ws],L,[],[],Wordlist_Split).
s_l([],_,[],Acc,Acc):- !.
s_l([],_,Acc_Small,Acc_Large,Wordlist_Split):-
	not(length(Acc_Small,0)),
	append(Acc_Large,[Acc_Small],Acc_Large1),
	s_l([],_,[],Acc_Large1,Wordlist_Split).
s_l([W|Ws],Previous_Length,Acc_Small,Acc_Large,Wordlist_Split):-
	length(W,Current_Length),
(	Current_Length =:= Previous_Length ->
	append(Acc_Small,[W],Acc_Small1),
	s_l(Ws,Current_Length,Acc_Small1,Acc_Large,Wordlist_Split)
;	append(Acc_Large,[Acc_Small],Acc_Large1),
	s_l(Ws,Current_Length,[W],Acc_Large1,Wordlist_Split)
).
%%%

%%% Removes one layer of nesting from a list
collapse(List,Collapsed):- clpse(List,[],Collapsed).
clpse([],Acc,Acc):- !.
clpse([E|Es],Acc,Collapsed):-
	append(Acc,E,Acc1),
	clpse(Es,Acc1,Collapsed).
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WORD INSERTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Takes a (W)ordlist, and (S)lot list and binds every word to a slot until the puzzle is finished
insert_words([],_):- !.
insert_words([W|Ws], Current_Slotlist):- 
	bind_word(W,Current_Slotlist,Partial_Slotlist),
	insert_words(Ws, Partial_Slotlist).
%%%

%%% Takes a word and attempts to bind it to a slot
bind_word(W,[S|Ss],Slots):- bind_w(W,[S|Ss],[],Slots).
bind_w(_,[],Acc,Acc).
bind_w(W,[S|Ss],Acc,Slots):-
	(W = S,					% Does the word bind to the next slot?
	append(Acc,[S],Acc1),		% YES Add the bound slot to the accumulator 
	append(Acc1,Ss,Acc2),		% Add the rest of the slots to the accumulator
	bind_w(_,[],Acc2,Slots)) 	% Exit with success
	;	
	(append(Acc,[S],Acc1),		% NO Word doesn't bind, but there are slots left, append this unbound slot to the accumulator
	bind_w(W,Ss,Acc1,Slots)),	% Move to the next slot
	not(length(Ss,0)).
%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PUZZLE AND SLOTLIST PREP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Does the second step of finding slots for vertical words, appending this new list of slots onto the existing horizontal slot list
%   Can not fail.
fill_slots_V([],Current_Slots,Current_Slots).
fill_slots_V([R|Rs],Current_Slots,New_Slots):-
	bind_slot(R,Slots),
	append(Current_Slots,Slots,Current_Slots1),
	fill_slots_V(Rs,Current_Slots1,New_Slots).
%%%

%%% Performs the initial filling of the logical variables into the spaces where letters will go, and creates 
%   a list of possible slots for words to go into (returns the filled puzzle and the Horizontal slot list).
%   Can not fail.
fill_slots_H(Puzzle,Slots,NewPuzzle):-
	f_s_H(Puzzle,[],[],Slots,NewPuzzle).
f_s_H([],SAcc,NPAcc,SAcc,NPAcc).
f_s_H([R|Rs],Slots_Acc,New_Puzzle_Acc,Slots,New_Puzzle):-
	bind_new(R,Bound_R),									% Fill row with logical variables
	append(New_Puzzle_Acc,[Bound_R],New_Puzzle_Acc1),			% Add this filled row to the new puzzle accumulator
	bind_slot(Bound_R,Bound_S),								% Get all the word slots from the filled row
	append(Slots_Acc,Bound_S,Slots_Acc1),					% Append these slots to the slot accumulator
	f_s_H(Rs,Slots_Acc1,New_Puzzle_Acc1,Slots,New_Puzzle).
%%%	

%%% Given a row of already bound logical variables, form slots for words to be inserted and return the whole 
%   list of them
%   Can not fail.
bind_slot(Row,Slots):- b_s(Row,[],[],Slots).
b_s([],[],Acc,Acc):- !.
b_s([],Current_Slot,Acc,Slots):-
	length(Current_Slot,L),
(	L < 2 -> b_s([],[],Acc,Slots)
;	append(Acc,[Current_Slot],Acc1),
	b_s([],[],Acc1,Slots)
).
b_s([S|Ss],Current_Slot,Acc,Slots):-
(	S == # ->
	length(Current_Slot,L), 
	(	L < 2 ->
		b_s(Ss,[],Acc,Slots)
	;	append(Acc,[Current_Slot],Acc1),
		b_s(Ss,[],Acc1,Slots)
	)
;	append(Current_Slot,[S],New_Current_Slot),
	b_s(Ss,New_Current_Slot,Acc,Slots)
).	
%%%

%%% Given a row of characters, create free logical variables for every space and return this new row
%   Can not fail.
bind_new(Row,NewRow):- bind_n(Row,[],NewRow).
bind_n([],Acc,Acc):- !.
bind_n([S|Ss],Acc,New):-
(	S = '_' ->
	length(E,1), 			% this creates a free variable
	append(Acc,E,Acc1),
	bind_n(Ss,Acc1,New) 
;	append(Acc,[S],Acc1), 	% ignore anything that isn't a free space (letters or hashes)
	bind_n(Ss,Acc1,New)	
).
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC. PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Toggle between flipped/not-flipped
flip(Puzzle,Is,Flipped):-
(	Is =:= 0 ->
	flipV(Puzzle,Flipped)
;	flipH(Puzzle,Flipped)
).
%%%
%%% Flip the puzzle for vertical input
flipV(P,R):-
	transpose(P,R).
%%%
%%% Flips a flipped puzzle back to normal for horizontal input
flipH(P,R):-
	transpose(P,R1),
	transpose(R1,R2),
	transpose(R2,R).
%%%

%%% Performs a quick sort by length of elements, in descending order
%%% from http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
pivot(_,[],[],[]).
pivot(H,[X|T],[X|L],G):- 
	length(H,Hl),
	length(X,Xl),
	Xl=<Hl,pivot(H,T,L,G).
pivot(H,[X|T],L,[X|G]):- 
	length(H,Hl),
	length(X,Xl),
	Xl>Hl,pivot(H,T,L,G).

qsort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivot(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
%%%

%%% Performs a quick sort by length of elements, in ascending order
%%% from http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
pivot_a(_,[],[],[]).
pivot_a(H,[X|T],[X|L],G):- 
	length(H,Hl),
	length(X,Xl),
	Xl>=Hl,pivot_a(H,T,L,G).
pivot_a(H,[X|T],L,[X|G]):- 
	length(H,Hl),
	length(X,Xl),
	Xl<Hl,pivot_a(H,T,L,G).

qsort_a(List,Sorted):-q_sort_a(List,[],Sorted).
q_sort_a([],Acc,Acc).
q_sort_a([H|T],Acc,Sorted):-
	pivot_a(H,T,L1,L2),
	q_sort_a(L1,Acc,Sorted1),q_sort_a(L2,[H|Sorted1],Sorted).
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%