% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.

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
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.
solve_puzzle(Puzzle,[],Puzzle).

solve_puzzle(Puzzle0, WordList, Puzzle) :-
	% Eg. [_G15092255, _G15092258, #, #, _G15092285, _G15092300] -> 
    %   [[_G15092255, _G15092258],[_G15092285, _G15092300]]

	create_slots_horizontal(Puzzle0, [], HorizontalSlots),
	
	print(SortedList),
	
	!,
	Puzzle = Puzzle0.
	


fill_slot(Word, Slot, WordSlot):-
	length(Word, WordLen),
	length(Slot, WordLen),
	WordSlot = Word,
	!.

	
	
create_slots_horizontal([],PuzzleSlots,PuzzleSlots).
create_slots_horizontal([Row|Rows],Slots,PuzzleSlots) :-
	slot_row(Row, RowAcc),
	append(Slots, [RowAcc], PuzzleSlots0),
	create_slots_horizontal(Rows, PuzzleSlots0, PuzzleSlots).

create_acc_slots_horizontal([],[],[],[]).

create_acc_slots_horizontal([Row|Rows], Acc, Slots, PuzzleSlots) :-
	slot_row(Row, Acc),
	append(Slots, Acc, PuzzleSlots),
	create_acc_slots_horizontal(Rows,[],PuzzleSlots).

% Eg. ['_','_',#,#,'_','_'] -> [_G15092255, _G15092258, #, #, _G15092285, _G15092300]
slot_row(Row, UnboundRow) :-
	% Only need to do it once per row
	once(slot_acc_row(Row, [], UnboundRow)).

slot_acc_row([],UnboundRow,UnboundRow).

slot_acc_row(Row, RowAcc, UnboundRow):-

	(	dif(Row, [])
		->
	
			slot_word(Row, UnboundWord),
			append(RowAcc,[UnboundWord],UnboundRow0),
			length(UnboundWord, WordLen),
			N is WordLen + 1,
			take(N,Row,Back),
			remove_hash(Back, HashNum),
			take(HashNum, Back, NewBack)
			
			
			% (	N > RowLen
			
			% 	->	slot_acc_row([], UnboundRow0, UnboundRow)
			% 	;	take2(N,Row, Back),
			% 		slot_acc_row(Back, UnboundRow0, UnboundRow)

			% )
		
		
	),
	slot_acc_row(NewBack, UnboundRow0, UnboundRow).



slot_word(Row, UnboundWord):-
	length(Row, RowLen),
	RowLen > 0,
	once(slot_acc_word(Row,[],UnboundWord)).

slot_acc_word([],UnboundWord,UnboundWord).

slot_acc_word([Char|Chars], WordAcc,UnboundWord) :-

	(
		Char = #
		->	
			(	length(WordAcc, Len),
				Len =:= 0
		
				->	slot_acc_word(Chars, WordAcc, UnboundWord)
				;	slot_acc_word([],WordAcc, UnboundWord)
			)
		;	
			(
				is_alpha(Char)
				->	append(WordAcc, [Char], UnboundWord0),
					slot_acc_word(Chars,UnboundWord0,UnboundWord)
				;	length(Unbound, 1),
					append(WordAcc, Unbound, UnboundWord0),
					slot_acc_word(Chars,UnboundWord0,UnboundWord)
			
			
			)

	).

			
			

remove_hash(List, Hash) :-
	once(remove_acc_hash(List,0, Hash)).

remove_acc_hash([#|List], HashAcc, TotalHash):-
	TotalHash0 is HashAcc + 1,
	remove_acc_hash(List,TotalHash0,TotalHash).
remove_acc_hash(_, TotalHash,TotalHash).


	
take(N, List, Back) :-
	length(List, ListLen),
	(
		N =< ListLen
		->	length(Front,N),
			append(Front, Back, List)
		;
		
		take(ListLen, List, Back)
	).
take2(N, List, Back) :-
	length(Front,N),
	append(Front, Back, List).

