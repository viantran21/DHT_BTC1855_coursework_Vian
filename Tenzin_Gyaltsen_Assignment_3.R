#' Assignment 3.
#' Purpose: Play a game of Hangman with the user.
#' Plan:
#' 1. Select random word for the game from an external .txt file of words.
#' 2. Inform the user about game rules and preliminary information. Also,
#' save win and lose condition variables in the back end, along with
#' underscores for progress text and a blank string for past guesses.
#' 3. Create a while loop that contains the code needed for the game.
#' 4. In the loop, provide a summary of prgress and prompt the user for 
#' a single letter input. 
#' 5. If the guess is the exact word, the user wins and exits loop. If 
#' the guess is invalid, print an error and prompt the user again. 
#' 6. If the above checks are passed, then assess whether the letter 
#' input is in the answer. If so, update progress text and start next
#' round unless user has guessed the entire word (win). If the letter input is
#' not in the answer, decrement lives and update past guesses and start 
#' next round, unless user has now lost (all lives lost).