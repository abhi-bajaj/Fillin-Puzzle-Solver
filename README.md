# Objective
The objective of this project is to solve fillin crossword puzzles with Prolog. This project was a part of COMP30020 formal assessment.

# Fillin Puzzles
A fillin puzzle (sometimes called a fill-it-in) is like a crossword puzzle, except that instead of being given obscure clues telling which words go where, you are given a list of all the words to place in the puzzle, but not told where they go.
The puzzle consists of a grid of squares, most of which are empty, into which letters or digits are to be written, but some of which are filled in solid, and are not to be written in.
You are also given a list of words or multi-digit numbers to place in the puzzle. Henceforth we shall discuss the puzzles in terms of words and letters, but keep in mind that they can be numbers and digits, too. You must place each word in the word list exactly once in the puzzle,
either left-to-right or top-to-bottom, filling a maximal sequence of empty squares. Also, every maximal sequence of non-solid squares that is more than one square long must have one word from the word list written in it. Many words cross one another, so many of the letters in a horizontal word will also be a letter in a vertical word. For a properly constructed fillin
puzzle, there will be only one way to fill in the words (in some cases, the puzzle is symmetrical around a diagonal axis, in which case there will be two symmetrical solutions).

# The Program
The main predicate is written in the file proj2.pl.

The program supplies a predicate main(PuzzleFile, WordlistFile, SolutionFile) that reads in the puzzle file whose name is PuzzleFile and the word list file whose name is WordlistFile, solve the puzzle, and print out the result to the file SolutionFile.

The puzzle file contains a number of lines each with the same number of characters, to form a rectangle. The characters in this file should all be either an underline character (_) indicating a fill-able square, a hash character (#) indicating a solid, non-fill-able square, or a letter or digit, indicating a pre-filled square. The output SolutionFile has the same format (except that it is be filled, so it should not contain underlines). The word list file is simply a text file with one word per line.

