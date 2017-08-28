%   Project2 prolog
%   Jizhe Hou
%   734947
%   21/10/2016
%   libiary: clpfd 
%   use the fillin_starter.pl
%
%
%

% load the libiary for the transpose function
:- ensure_loaded(library(clpfd)).
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

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

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%


solve_puzzle(Puzzle, WordList, Solved):-
    change_to_var_puzzle(Puzzle, Solved),
    % transpose the Puzzle
	transpose(Solved, PuzzleTransposed),
    % according to the Vars_Puzzle to bulid the slot
	createSlots(Solved, [], Slots1),
    % according to the transpose Vars_Puzzle to bulid the slot
	createSlots(PuzzleTransposed, Slots1, Slots),
    % fill the words into the slots
	fillinSlots(Slots, WordList).   
    
    
    
    
% change_to_var_puzzle(Puzzle, Solved)
% copy the Puzzle to Solved, the thing which have been taken is 
% replace the '_' to logical var
change_to_var_puzzle([], []).
change_to_var_puzzle(Rows, Vars_Puzzle) :-
    maplist(row_to_vars, Rows, Vars_Puzzle).


% row_to_vars(Row, RowWithVars)
% split each row from the whole Puzzle
% for each row change the '_' to a logical var
row_to_vars([], []).
row_to_vars(Row, Vars_Row) :-
    maplist(change_underscore_to_var, Row, Vars_Row).


% change_underscore_to_var(Char, Var)
% if the char is '_', then change it to an logical var
% if not just keep the char itself
change_underscore_to_var('_', _).
change_underscore_to_var(Char, Char) :- Char \= '_'.


% tranform a puzzle to a list of slots which can be filled in with words
% createSlots(Puzzle, ExistSlots, Slots)
% combine the exist slots with the new slots together
createSlots([], ExistSlots, ExistSlots).
createSlots([Row|OtherRows], ExistSlots, Slots) :-
	processRow(Row, [], ExistSlots, TempSlots),
	createSlots(OtherRows, TempSlots, Slots).

% tranform a row to a list of slots which can be filled in with words
% processRow(PuzzleRow, CurrentWord, ExistSlots, Slots)
% CurrentWord	The current part of a word that is being built
% ExistSlots	A list of slots which already been created
% Slots	        combine all the slots in this row together


unify(Word1, Word1).

processRow([], CurrentWord, ExistSlots, Slots) :-
	(	CurrentWord == []
	->	unify(Slots, ExistSlots)
	;	(	length(CurrentWord, Currentlength), 	Currentlength > 1
		->	append(ExistSlots, [CurrentWord], Slots)
		;	unify(Slots, ExistSlots)
		)
	).
    
processRow([Char|Rest], CurrentWord, ExistSlots, Slots) :-
	(	Char == '#'
	->	(	CurrentWord == []
		->	processRow(Rest, [], ExistSlots, Slots)
		;	(	length(CurrentWord, Currentlength), Currentlength > 1
			->	append(ExistSlots, [CurrentWord], TempSlots),
				processRow(Rest, [], TempSlots, Slots)
			;	processRow(Rest, [], ExistSlots, Slots)
			)
		)
	;	append(CurrentWord, [Char], NewCurrentWord),
		processRow(Rest, NewCurrentWord, ExistSlots, Slots)
	).

% Fills in all the slots with the given wordList
% It finds the slot with the least number of combos
% It then tries to fill the slot with a word
% fillSlots(Slots, WordList)

fillinSlots([], _).

fillinSlots([Slot|Slots], WordList) :-
	getCombos(Slot, WordList, 0, Matches),
	leastCombos(Slots, WordList, Matches, Slot, BestSlot, [], NewSlots),
	fillSlot(BestSlot, WordList, [], NewWordList),
	fillinSlots(NewSlots, NewWordList).
    

    
% calculate this slot can be filled in how many words
% getCombos(Slot, WordList, CurrentMatches, TotalMatches)
% Slot				The slot we want to know
% CurrentMatches	keep the number for now
% TotalMatches		The total number of matches

getCombos(_, [], CurrentMatches, CurrentMatches).
getCombos(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	canUnify(Slot, Word)
	->	getCombos(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	;	getCombos(Slot, OtherWords, CurrentMatches, TotalMatches)
	).
    
% for each slot, choose the least number of possible
% add them together to the best slovtion

leastCombos([], _, Answer, CurrentSlot, CurrentSlot,
	NewSlotsIn, NewSlotsIn) :-
	Answer > 0.
leastCombos([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
	BestSlot, NewSlotsIn, NewSlotsOut) :-
	getCombos(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
	->	append(NewSlotsIn, [CurrentSlot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, TotalMatches, Slot, BestSlot,
			NewSlotsTemp, NewSlotsOut)

	;	append(NewSlotsIn, [Slot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, LeastMatches, CurrentSlot,
			BestSlot, NewSlotsTemp, NewSlotsOut)
	).



% Checks if two lists can be unified
% canUnify(Word1, Word2)
% Word1	A word to try and unify with Word2
% Word2 A word to try and unify with Word1
canUnify([], []).
canUnify([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	canUnify(Word1, Word2)
	).


    
% use the wordlist to fillin the slot 
% then remove the word from the wordlist

fillSlot(Slot, [Word|WordList], NewWordListIn, NewWordListOut) :-
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).
