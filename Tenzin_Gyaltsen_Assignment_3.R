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

#' The reviewer is advised to save a copy of the csv data file in the same 
#' working directory as this file.

# Import words from text file into a data frame.
words <- read.table("words.txt", header = FALSE)

#' Select a random word answer from the vector of words within the data 
#' frame using the sample() function. 
answer <- sample(words$V1, 1)

#' Set the game parameters, including number of lives and win/lose
#' conditions.
lives <- 5
win <- FALSE

#' Create empty strings for game progression and past guesses list that 
#' will be updated as the game progresses.
placeholder <- ""
for (i in 1:nchar(answer)) {
  placeholder <- paste0(placeholder, "_")
}
past_guesses <- ""

#' Print introduction to user with game objective and brief rules.
print("Hello, welcome to Hangman!")
print(paste("Your goal is to guess the word by entering its letters or guessing it entirely. The word has", nchar(answer), "letters."))
print("You have 5 incorrect guesses before you lose!")

#' Begin game via a while loop that will run as long as lose and win 
#' conditions are not met.
while (lives != 0 & win != TRUE) {
  #' Print a summary of the game thus far each round.
  print(paste("Current Progress:", placeholder))
  print(paste0("Past Guesses:", past_guesses))
  print(paste("Lives:", lives))
  #' Prompt user for their guess and save into a variable.
  guess <- readline(prompt = "Please enter one letter or try to guess the word: ")
  #' Create if/else flow to assess different variations of the user's input.
  if (grepl(guess, answer) & nchar(guess) == nchar(answer)) {
    #' First assess whether the guess exactly matches the answer using the
    #' grepl() function. This function verifies that the guess is contained 
    #' within the answer and the additional check confirms that they are 
    #' the same number of characters. If this first check is true, set the
    #' win condition to TRUE to exit the loop/game.
    win <- TRUE
    # Print win statement.
    print(paste0("YOU WIN! The word was ", answer, "."))
  } else if (!grepl("[a-z]", guess, ignore.case = TRUE) || nchar(guess) != 1) {
    #' Secondly, check that the input is valid. We again use the grepl() 
    #' function to verify that the input does not contain a character from a-z, 
    #' ignoring case. We also verify that the length of the input is not only one 
    #' character. If either of these conditions are met, an error message 
    #' is printed and the loop is re-run (next round).
    print(paste(guess, "is not a single letter."))
  } else {
    #' If the user inputs enters a single letter, then the program will
    #' flow into this else statement.
    if (grepl(guess, answer, ignore.case = TRUE) & !grepl(guess, placeholder)) {
      #' If the letter guess, ignoring case, is contained in the answer,
      #' and the letter guess is not already in the placeholder/progress
      #' text, the program will flow into this if statement.
      for (i in 1:nchar(placeholder)) {
        #' Create a for loop that loops through the placeholder.
        if (tolower(guess) == substring(answer,i,i)) {
          #' If the letter guess, converted to lowercase, matches the 
          #' answer at any position, that same position in the placeholder
          #' is changed from an underscore to the letter guess using the
          #' substring() function.
          substr(placeholder, i, i) <- tolower(guess)
        } 
      }
      # Print correct guess statement and placeholder progress.
      print("Correct guess! Here is your progress:")
      print(placeholder)
      if (placeholder == answer) {
        #' If the correct guess completes the placeholder, then feed into this if statement
        #' update the win condition to finish the game and exit the loop.
        win <- TRUE
        # Print win statement.
        print(paste0("YOU WIN! The word was ", answer, "."))
      } 
    } else {
      #' If the valid letter guess is not in the answer, the program feeds
      #' into this else statement.
      #' Decrement lives by 1. If lives becomes 0, then the loop will not
      #' run again since the lose condition is met.
      lives <- lives - 1
      if (lives == 0) {
        #' If the user is now out of lives, the program feeds into this
        #' if statement. The lose statement and progress thus far are
        #' printed. 
        print(paste0("YOU LOSE! The word was ", answer, ". Here was your progress:"))
        print(placeholder)
      } else {
        #' If the user still has lives left, then the incorrect guess
        #' statement is printed along with progress text and lives
        #' remaining.
        print("Incorrect or repeat guess! Here is your progress:")
        print(placeholder)
        print(paste0("You have ", lives, " life/lives left!"))
        if (!grepl(guess, past_guesses)) {
          #' If the incorrect letter guess is not already in the past guesses
          #' vector, then the new incorrect letter guess is added to the
          #' past guesses vector using the paste() function.
          past_guesses <- paste(past_guesses, guess)
        }
      }
    }
  }
  #' Below function is necessary to ensure that precedingtext is visible
  #' before the screen is cleared in the below line.
  Sys.sleep(3)
  # To clear the screen between each round of the game, aesthetic purpose.
  cat("\014")
}
print("Thanks for playing!")