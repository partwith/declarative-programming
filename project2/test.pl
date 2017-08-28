solve_puzzle(Puzzle, WordList) :-
	% Grab a transposed version of the puzzle
	transpose(Puzzle, PuzzleTransposed),

	% Build the list of slots
	buildSlots(Puzzle, [], Slots1),
	buildSlots(PuzzleTransposed, Slots1, Slots),

	% Put words into slots
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
	% Workout how many combos the first word has
	getCombos(Slot, WordList, 0, Matches),

	% Calculate the slot with the least matches
	leastCombos(Slots, WordList, Matches, Slot, BestSlot, [], NewSlots),

	% Fill a slot
	fillSlot(BestSlot, WordList, [], NewWordList),

	% Fill remaining slots
	fillSlots(NewSlots, NewWordList).
    
leastCombos([], _, Answer, CurrentSlot, CurrentSlot,
	NewSlotsIn, NewSlotsIn) :-
	% No point trying to fill the slot if nothing can fill it
	Answer > 0.
leastCombos([Slot|OtherSlots], WordList, LeastMatches, CurrentSlot,
	BestSlot, NewSlotsIn, NewSlotsOut) :-
	% Workout how many combos there are for the given slot
	getCombos(Slot, WordList, 0, TotalMatches),
	(	TotalMatches < LeastMatches
	->	% This is the new best slot, update our values
		append(NewSlotsIn, [CurrentSlot], NewSlotsTemp),
		leastCombos(OtherSlots, WordList, TotalMatches, Slot, BestSlot,
			NewSlotsTemp, NewSlotsOut)

	;	% Old Slot is better, ignore this slot
		append(NewSlotsIn, [Slot], NewSlotsTemp),
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
	% Attempt to fill the slot
	unify(Slot, Word),
	append(WordList, NewWordListIn, NewWordListOut);

	% Failure, try next item
	append(NewWordListIn, [Word], NewWordListTemp),
	fillSlot(Slot, WordList, NewWordListTemp, NewWordListOut).