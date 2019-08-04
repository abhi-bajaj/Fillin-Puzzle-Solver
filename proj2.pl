/*
	Solution to COMP30020 2018s2 Project2
    prepared by Arpit Bajaj, bajaja@student.unimelb.edu.au
    October 2018
	
	Skeleton Code provided by Peter Schachte

	Purpose:

	This program aims to solve Fillin Puzzles. A fillin puzzle is similar to a 
	crossword puzzle. You are provided with a list of all the words to place in 
	the puzzle, but not told where they go.

	The puzzle consists of a grid of squares, most of which are empty, 
	into which letters or digits are to be written, but some of which are 
	filled in solid, and are not to be written in.

	For more information about fillin puzzles, refer to:
	https://en.wikipedia.org/wiki/Fill-In_(puzzle)

*/ 

/*
	The SWI Prolog library provides two different, incompatible 
	transpose/2 predicates, and unfortunately autoloads the wrong one by
	default. Therefore the clpfd library is used, which defines the predicate 
	transpose(Matrix0, Matrix) that holds when Matrix0 and Matrix are lists of
	lists, and the “columns” of each are the “rows” of the other.
*/
:- ensure_loaded(library(clpfd)).

/*****************************************************************************/
% 									Main
/*****************************************************************************/	

/*
	This main/3 requires 3 filenames as input.

	The first input is the PuzzleFile. The PuzzleFile must meet the following
	constraints to be cosidered valid. Empty squares must be represented by '_'
	characters, filled in squares should be represented by the character itself
	and solid squares must be represented by # characters. Furthermore, the
	grid formed by these squares must be a square grid.

	The second input is the Word File. The WordFile must contain all of the 
	words that belong in the puzzle. These should be seperated by a newline 
	character.

	The final input is the output file, to which the solution will be written 
	to.

	This predicate reads in the PuzzleFile and WordFile. After 
	ensuring the puzzle file is valid, it attempts to solve the puzzle.
	If it is successful in solving the puzzle, the solution is written out
	to the SolutionFile.
*/
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

/*****************************************************************************/
% 							Solve Puzzle Predicates
/*****************************************************************************/	

/*
	The solve_puzzle/3 predicate will be True if Puzzle0 is succesfully solved
	with regards to WordList, after which Puzzle will be the solved version of 
	the puzzle.

	The predicate works as such :

	1.	Sort the initial WordList in regards to the length of each word.
	2.	Fill the initial Puzzle with logical unbound variables, denoted by
	   	PuzzleLogical.
	   	Eg: [[#,'_','_', #], [#, #,'_','_']] -> [[#,_409,_302,#], [#,#_211,_231]]
	
	3.	Create a list of slots arising from the PuzzleLogical.
	   	Eg: [[#,_409,_302, #], [#, #,_211,_231]] -> [[_409, _302], [_211,_231]]

	4. 	Transpose the PuzzleLogical into its vertical form (PuzzleVerical)
	   	and perform step 3 on this vertical representation, appending the new
	   	list of slots to the previous list, resulting in Slots.

	5. 	Sort the Slots in regards to the length of each slot.

	6. 	Attempt to fill in each slot to solve the puzzle.

*/
solve_puzzle(Puzzle0, WordList, Puzzle) :-
	sort_list_len(WordList, SortedWordList),
	fill_puzzle_logical(Puzzle0, PuzzleLogical),
	create_slots(PuzzleLogical, [], SlotList),
	transpose(PuzzleLogical, PuzzleVertical),
	create_slots(PuzzleVertical, SlotList, Slots),
	sort_list_len(Slots, SortedSlots),	
	fill_slot_list(SortedSlots, SortedWordList),
	% ! used to avoid backtracking
	!, 
	Puzzle = PuzzleLogical.

/*
	The fill_slot_list/2 predicate takes in a list of slots (Slots) and a list 
	of words (Words). It then attempts to fill in each slot with a word in 
	order to complete the puzzle.

	The algorithm was adapted from Hint 6 of the specification and works as 
	such:

	1.	Generate all possible matches for each slot, denoted by
		Combos. Each match is denoted by NumberOfMatches-Slot where 
		NumberOfMatches is how many possible words fit in and 
		are associated with that Slot. 

	2.	Use the inbuilt keysort to get the slot with the least amount of 
		matches to the front.
	3.	Fill in the slot with the least matches.
	4.	Repeat steps 1-3 for the remaining words and slots.
	5.	Prolog will automatically backtrack on failures and attempt 
		different permutations if needed.
	6.	The puzzle is deemed solved when there are no more slots to fill, and
		no words left to enter.
*/
fill_slot_list([],[]).
fill_slot_list(Slots, WordList) :-
	generate_all_combos(Slots, WordList, [], Combos),
	keysort(Combos, [HeadSorted|_SortedCombos]),
	HeadSorted = _-CurrSlot,

	% Bind the CurrSlot
	member(CurrSlot, WordList),
	
	% exclude took so long to figure out. Was initially deleting the CurrSlot,
	% which was resulting in deletion of the unbound slots as well as the
	% exact slot.
	exclude(==(CurrSlot), Slots, NewSlotList),
	exclude(==(CurrSlot), WordList, NewWordList),
	fill_slot_list(NewSlotList, NewWordList).

/*
	generate_all_combos/4 generates all possible matches for each slot, denoted
	by Combos. Each match is denoted by NumberOfMatches-Slot where 
	NumberOfMatches is how many possible words fit in and 
	are associated with that Slot.

	The algorithm works as such :

	Generate the NumberOfMatches-Slot for one slot, append it to the 
	accumulated list, and recursively do this for all slots.
*/
generate_all_combos([], _, Combos, Combos).
generate_all_combos([Slot|Slots], WordList, ComboAcc, Combos):-
	combo_slot_word(Slot, WordList, ComboCurrent),
	append(ComboAcc, [ComboCurrent], CombosAcc0),
	generate_all_combos(Slots, WordList, CombosAcc0, Combos).

/*
	To get Number of Matches (SlotFits) for the Slot, count the matches via an 
	accumulator in the combo_slot_word_acc/4 predicate
*/
combo_slot_word(Slot, WordList, SlotFits-Slot):-
	combo_slot_word_acc(Slot, WordList, 0, SlotFits).

/* Base Case: No more words left to try against the current slot */
combo_slot_word_acc(_, [], SlotFits, SlotFits).
combo_slot_word_acc(Slot, [Word|Words], SlotFitsAcc, SlotFits) :-
	(
		% Copy as not to bind it straight to the slot
		copy_term(Slot, CopySlot),
		
		% If the word can be binded to the current slot, add 1 to the
		% accumulator. Then check if slot can be binded to the rest of the 
		% words
		member(Word, [CopySlot])
		->	SlotFits0 is SlotFitsAcc + 1,
			combo_slot_word_acc(Slot, Words, SlotFits0, SlotFits)
		;	combo_slot_word_acc(Slot, Words, SlotFitsAcc, SlotFits)	
	).

/*****************************************************************************/
%		Predicates to convert puzzle rows into rows of logic variables
% 	Eg: [[#,'_','_', #], [#, #,'_','_']] -> [[#,_409,_302,#], [#,#_211,_231]]
/*****************************************************************************/

/*
	fill_puzzle_logical/2 fills in the puzzle with logic variables via
	a call to fill_puzzle_acc_logical/3 predicate which makes use of an
	accumulator. 
*/
fill_puzzle_logical(Puzzle0, PuzzleLogical) :-
	fill_puzzle_acc_logical(Puzzle0, [], PuzzleLogical).

/*
	fill_puzzle_logical/3 fills in the puzzle with logic variables via
	an accumulator. 

	Base Case: When there are no rows left to fill.
	
	Otherwise generate logic variables for current row, append it to the 
	running accumulator and then move onto the next row.

*/
fill_puzzle_acc_logical([], PuzzleLogical, PuzzleLogical).
fill_puzzle_acc_logical([Row|Rows], LogicalAcc, PuzzleLogical):-
	logical_row(Row, LogicalRow),
	append(LogicalAcc, [LogicalRow], PuzzleLogical0),
	fill_puzzle_acc_logical(Rows, PuzzleLogical0, PuzzleLogical).

/*
	logical_row/2 fills in the row of the puzzle with logic variables via
	a call to logical_row/3 predicate which makes use of an accumulator. 
*/
logical_row(Row, LogicalRow) :-
	logical_acc_row(Row, [], LogicalRow).

/*
	fill_puzzle_logical/3 fills in the row with logic variables via
	an accumulator. 

	Base Case: When there are no characters left in the row to fill.
	
	Otherwise generate logic variables for each, append it to the 
	running accumulator and then move onto the next character.

*/
logical_acc_row([], LogicalRow, LogicalRow).
logical_acc_row([Char|Chars], LogicalRowAcc, LogicalRow) :-
	( 
		% If the current Char is a hash character, append it straight to
		% the accumulator.
		Char = #
		->	append(LogicalRowAcc, [Char], LogicalRow0)
		;	(
				% Otherwise, if current Char is an underscore character,
				% create an Unbound variable and append that variable to the
				% accumulator.
				Char = '_'
				->	length(Unbound, 1),
					append(LogicalRowAcc, Unbound, LogicalRow0)	
				% Since neither condition passed, this must be a filled in
				% character, append it straight to the accumulator.
				;	append(LogicalRowAcc, [Char], LogicalRow0)
			
			)
	),
	logical_acc_row(Chars, LogicalRow0, LogicalRow).


/*****************************************************************************/
%			Predicates to convert puzzle rows into a list of slots
% 	Eg: [[#,_409,_302, #], [#, #,_211,_231]] -> [[_409, _302], [_211,_231]]
/*****************************************************************************/

/*
	create_slots/3 converts a list of rows in the puzzle into a list of slots.

	Base Case: There are no rows left to complete

	Otherwise:
	
	1.	Ensure the length of the current row is higher than 1.
	2.	If so, get the list of slots for the current row, append it to
		the accumulator and move on to the next row.
	3.	If length is less than one, just move straight onto the next row.

*/
create_slots([], PuzzleSlots, PuzzleSlots).
create_slots([Row|Rows], Slots, PuzzleSlots) :-
	length(Row, RowLen),
	(
		RowLen > 1
		->	slot_row(Row, RowAcc),
			append(Slots, RowAcc, PuzzleSlots0),
			create_slots(Rows, PuzzleSlots0, PuzzleSlots)
		;	create_slots(Rows, Slots, PuzzleSlots)
			
	).
	
/*
	slot_row/2 converts a row of logic variables to a row of slots via
	a call to slot_acc_row/3 predicate which makes use of an accumulator. 
*/
slot_row(Row, UnboundRow) :-
	slot_acc_row(Row, [], UnboundRow).

/*
	Base Case: There are characters left in the row

	Otherwise:

	1.	Remove the hashes from the start of the row.
	2.	Get a word from the row (all characters until it hits a hash).
	3.	If the length of the word is greater than one, append it to the running
		accumulator.
	4.	Remove the word from the rest of the row.
	5.	Remove the hashes from the rest of the row.
	6. 	Repeat until row is exhausted.
*/
slot_acc_row([], UnboundRow, UnboundRow).
slot_acc_row(Row, RowAcc, UnboundRow):-
	remove_hash(Row, InitialHashNum),
	take(InitialHashNum, Row, InitRow),
	slot_word(InitRow, UnboundWord),
	length(UnboundWord, WordLen),
	(
		WordLen > 1
		->	append(RowAcc, [UnboundWord], UnboundRow0)
		;	UnboundRow0 = RowAcc
	),
	take(WordLen, InitRow, Back),
	(
		Back == []
		%	No Hashes to remove from an empty list
		->	HashNum = 0
		;	remove_hash(Back, HashNum)
	),
	take(HashNum, Back, NewBack),
	slot_acc_row(NewBack, UnboundRow0, UnboundRow).

/*
	slot_word/2 extracts a slot from a row of logic variables via
	a call to slot_acc_word/3 predicate which makes use of an accumulator. 
*/
slot_word(Row, UnboundWord):-
	slot_acc_word(Row, [], UnboundWord).

/*
	slot_acc_word/3 extracts a slot from a row of logic variables via a 
	running accumulator.

	Base Case:	There are no characters left in the row. Also called when a 
				Hash is hit.
	
	Otherwise:

	1.	If a hash is hit, the UnboundWord is complete.
	2.	Otherwise, add the current character to the running accumulator and
		move to next character.
*/
slot_acc_word([], UnboundWord, UnboundWord).
slot_acc_word([Char|Chars], WordAcc, UnboundWord) :-
	(
		Char == '#'
		->	slot_acc_word([] ,WordAcc, UnboundWord)
		;	append(WordAcc, [Char], UnboundWord0),
			slot_acc_word(Chars, UnboundWord0, UnboundWord)
	).

/*****************************************************************************/
% 							  Helper Predicates
/*****************************************************************************/		

/*
	remove_hash/2 predicate denotes the number of hashes at the start of a 
	list of characters. 

	If the first character is hash, it makes a call to the remove_acc_hash/3
	predicate to count up the rest of the hashes via an accumulator.
 
	Otherwise, Hash is simply set to 0.
*/
remove_hash([Char|Chars], Hash) :-
	(
		Char == '#'
		->	remove_acc_hash(Chars, 1, Hash)
		; Hash = 0
	).

/*
	remove_acc_hash/3 predicate counts the number of hashes at the start of
	a list of characters.
	
	Base Case: There are no more characters left, also called when a non-hash
	character is hit.

	Otherwise:
	
	1. 	If the current character is a hash, add 1 to the running accumulator,
		and move on to the next character.
	2.	Otherwise, a non hash character is hit, so no need to keep counting.	
*/
remove_acc_hash([], TotalHash, TotalHash).
remove_acc_hash([Char|Chars], HashAcc, TotalHash):-
	(
		Char == '#'
		->	TotalHash0 is HashAcc + 1,
			remove_acc_hash(Chars, TotalHash0, TotalHash)
		;	remove_acc_hash([], HashAcc, TotalHash)
	).
	
/*
The following two predicates were adapted from
https://stackoverflow.com/questions/27760689/
								sort-a-list-of-structures-by-the-size-of-a-list
*/

/*
	el_keyed/2 gets the length of El, and joins the length to El via a dash.
*/
el_keyed(El, N-El) :-
	length(El, N).
 
 /*
	sort_list_len/3 sorts the elements of a list in descending order,
	in regards to the length of each element.
 */
sort_list_len(Els, ElsS) :-
	maplist(el_keyed, Els, KVs),
	keysort(KVs, KVsS),
	reverse(KVsS, RVsS),
	maplist(el_keyed, ElsS, RVsS).

/*
	take/3 is adapted from the lecture slides of COMP30020.
	Takes off the first N elements from a List.
*/
take(N, List, Back) :-
	length(Front,N),
	append(Front, Back, List).


/*****************************************************************************/
%							  Provided Predicates
/*****************************************************************************/

/*
	read_file/2, opens the FileName and reads all lines from the file into 
	Content before closing the Stream.
*/
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

/*
	read_lines/2 reads the input Stream into Content. 
	Stream is the opened file, and Content will be a list of strings.
	Reads all lines from Stream into Content.
*/
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

/*
	read_line/3 reads in one line from Stream into Line, with Last Denoting
	if an EOF character is hit. Also moves on to the next line if a newline
	character is hit.
*/ 
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

/*
	print_puzzle/2 opens the SolutionFile, and writes out each row in the
	Puzzle to the File.
*/
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

/*
	print_row/2 prints each character in the Row out to Stream
*/
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

/*
	put_puzzle_char/3 writes '_' to stream if Char is a variable, otherwise
	writes Char itself.
*/
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

/*
	valid_puzzle/2 takes in the rows of a puzzle and ensure the puzzle is a 
	square. Ie, ensures the length of each row is the same. 
*/
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

/*
	samelength/2 holds when the length of both Lists is the same.
*/
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).
