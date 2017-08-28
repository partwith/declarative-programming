
:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist),
	print_puzzle(SolutionFile, Puzzle).
    
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
	; Char = '_'
	->	Line = [NewVariable|Line1],
	    read_line(Stream, Line1, Last)
	; Line = [Char|Line1],
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



solve_puzzle(Puzzle, WordList) :-
	transpose(Puzzle, PuzzleTransposed),
	buildSlots(Puzzle, [], Slots1),
	buildSlots(PuzzleTransposed, Slots1, Slots),
	fillSlots(Slots, WordList).

buildSlots([], InputSlots, InputSlots).
buildSlots([Row|OtherRows], InputSlots, OutputSlots) :-
	processRow(Row, [], InputSlots, TempSlots),
	buildSlots(OtherRows, TempSlots, OutputSlots).

processRow([], CurrentWord, InputSlots, OutputSlots) :-
	(	CurrentWord == []
	->	unify(OutputSlots, InputSlots)
	;	(	length(CurrentWord, Currentlength), 	Currentlength > 1
		->	append(InputSlots, [CurrentWord], OutputSlots)
		;	unify(OutputSlots, InputSlots)
		)
	).
processRow([Char|Rest], CurrentWord, InputSlots, OutputSlots) :-
	(	Char == '#'
	->	(	CurrentWord == []
		->	processRow(Rest, [], InputSlots, OutputSlots)
		;	(	length(CurrentWord, Currentlength), Currentlength > 1
			->	append(InputSlots, [CurrentWord], TempSlots),
				processRow(Rest, [], TempSlots, OutputSlots)
			;	processRow(Rest, [], InputSlots, OutputSlots)
			)
		)
	;	append(CurrentWord, [Char], NewCurrentWord),
		processRow(Rest, NewCurrentWord, InputSlots, OutputSlots)
	).

fillSlots([], _).
fillSlots([Slot|Slots], WordList) :-
	getCombos(Slot, WordList, 0, Matches),
	leastCombos(Slots, WordList, Matches, Slot, BestSlot, [], NewSlots),
	fillSlot(BestSlot, WordList, [], NewWordList),
	fillSlots(NewSlots, NewWordList).

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


getCombos(_, [], CurrentMatches, CurrentMatches).
getCombos(Slot, [Word|OtherWords], CurrentMatches, TotalMatches) :-
	(	canUnify(Slot, Word)
	->	getCombos(Slot, OtherWords, CurrentMatches+1, TotalMatches)
	;	getCombos(Slot, OtherWords, CurrentMatches, TotalMatches)
	).


canUnify([], []).
canUnify([W1|Word1], [W2|Word2]) :-
	(	(W1 == W2; var(W1); var(W2))
	->	canUnify(Word1, Word2)
	).


unify(Word1, Word1).


fillSlot(Slot, [Word|WordList], NewWordListIn, NewWordListOut) :-
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).