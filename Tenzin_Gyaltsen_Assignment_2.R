# Assignment 2.
# Purpose: Take a 3 digit positive number from the user and print whether or 
# not it is an Armstrong number.

#' Rough Plan:
#' 1. Prompt the user to enter a 3 digit positive number.
#' 2. Check if the input is numeric.
#' 3. Check if the input is positive and 3 digits.
#' 4. Check if the number is an Armstrong number.
#' 5. Display the result with an appropriate message.

#' 1. Prompt user to enter a 3 digit answer which is saved into the variable
#' "answer". 
answer_original <- readline(prompt = "Please enter a 3 digit positive number: ")

#' 2. Check if the input is numeric by first forcing the input to numeric
#' type using the "as.numeric()" function. If the value becomes NA then the
#' input is not numeric. This can be checked using an if statement and the 
#' "is.na()" function, with appropriate error message displayed if true.
answer_numeric <- as.numeric(answer_original)

if (is.na(answer_numeric)) {
  paste(answer_original, "is not numeric.")
} else if (answer_numeric < 99 || answer_numeric > 999 || answer_numeric%%1 != 0) {
  #' 3. Check if the input is not positive or not 3 digits (i.e., less
  #' than 99 and more than 999). Also check that the input is a whole number 
  #' (no decimals) by confirming that the input remainder is 0 when divided 
  #' by 1. If true, then print an appropriate error message.
  paste(answer_original, "is not a positive 3 digit whole number.")
} else {
  #' 4. Check if the number is an Armstrong number by first extracting each 
  #' digit character using the substring() function, converting them to 
  #' numerics, and saving them into separate variables. Then sum the digits 
  #' and assign the value to a new variable called "digit_sum". Finally, 
  #' create a nested if statement to evaluate whether the sum of the digits 
  #' is equal to the user's numerical input.
  digit1 <- as.numeric(substring(answer_original, 1, 1))
  digit2 <- as.numeric(substring(answer_original, 2, 2))
  digit3 <- as.numeric(substring(answer_original, 3, 3))
  digit_sum <- digit1^3 + digit2^3 + digit3^3
  if (digit_sum == answer_numeric) {
    #' 5. If the sum of the digits is equal to the user's number, display
    #' an appropriate message.
    paste(answer_original, "is an Armstrong number!")
  } else {
    #' 5. If the sum of the digits is not equal to the user's number, 
    #' display an appropriate message.
    paste(answer_original, "is not an Armstrong number.")
  }
}