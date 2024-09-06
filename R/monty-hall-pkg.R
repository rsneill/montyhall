#' @title
#'   Create a new game based on the Monty Hall Problem.
#'
#' @description
#'    `create_game()` generates a new game that consists of two doors 
#'    with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'   The function returns a character vector with three values
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'   Output: c("goat", "car", "goat")
#'
#' @export
#' 
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Player selects a door
#' 
#' @description
#'   'select_door()' randomly selects one of the three doors from 'create_game'.
#' 
#' @details
#'   This step simulates the constesant responding to the offer of three doors
#'   by selecting the one they hope will reveal the car.  The constestant has
#'   no prior knowledge of the prize locations.  The selection uses a random
#'   sample choice.  The sample seed can be set by adding set.seed(seed value)
#'   before the calling the function.
#' 
#' @param ... no arguments are used by this function.
#' 
#' @return 
#'   A numeric value representing the contestant selection is returned as the vector index of 'create_game()'
#' 
#' @examples
#'   select_door()
#'   Output: 2
#' 
#' @export
#' 
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Reveal a 'Zonk' (goat)
#' 
#' @description
#'  The host requests that one of the models/assistants reveal a losing
#'  door for the contestant.  The revealed door cannot reveal the car
#'  or include the contestants selection (a.pick).
#' 
#' @details
#'  The function evaluates the constestant selection.  If the contestant selected
#'  the car, the function will randomly reveal one of the other doors.  If the
#'  contestant selected a goat, the function will always reveal the other goat door.
#'  The reveal uses the 'create_game()' vector as the baseline and excludes the car and
#'  contestant selection.
#' 
#' @param game The returned value from 'create_game()' containing the vector of doors
#' @param a.pick The returned value from the 'select_door()' function containing the contestant selection
#' 
#' @return 
#'  A numeric value representing the revealed door is returned as the vector index of 'create_game()'
#' 
#' @examples
#'  open_goat_door( game, a.pick)
#'  Output: 2
#'  open_goat_door( c("goat", "car", "goat"), 2)
#'  Output: 1
#'
#' @export
#' 
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Contestant has the option to stay or switch doors after a goat is revealed
#'  
#' @description
#'  The contestant is given an option to change doors after a Zonk (goat) has been revealed.
#'
#' @param stay A logical value indicating whether the player will stay with their initial choice.
#' @param opened.door The revealed door not contains the prize.
#' @param a.pick The door initially chosen by the player.
#'
#' @return 
#'  The numeric vector index for the selected door.  If the player stays, the initial selection 
#'  is returned.  If the player switches, the remaining unopened door is returned.
#'
#' @examples
#' change_door(1, 3)
#'  Output: 2
#' 
#' change_door(2, 2)
#'  Output: 1
#'
#' @export
#' 
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}





#' @title
#'  Simulate the outcome of the game
#' 
#' @description
#'  The function determines the outcome of the game based on the final door selection.
#' 
#' @details
#'  The function calls each of the previous functions to simulate a game passing 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return
#'  A dataframe containing the result of each strategy (stay or switch) and the outcome (win or lose) of each.
#' 
#' @examples
#'  play_game()
#'   - create_game() output: c("goat", "car", "goat")
#'   - select_door() output: 1 
#'   - open_goat_door( c("goat", "car", "goat"), 1) output: 3
#'   - change_door( stay=T, 3, 1) output: 1
#'    - determine_winner( 1, c("goat", "car", "goat")) output: "lose"
#'   - change_door( stay=F, 3, 1) output: 2
#'    - determine_winner( 1, c("goat", "car", "goat")) output: "win"
#' Output:
#'   game.results("stay", "lose"
#'               "switch", "win")
#' @export
#' 
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Loop through a simulation the game n times and return the results as a dataframe.
#' 
#' @description
#'  The function simulates the game n times and returns the results as a dataframe.
#'  
#' @details
#'  The function uses a for loop to iterate through the number of games specified.  It 
#'  calls the 'play_game()' function to execute the game and collect the results.
#'  The function also uses the 'dplyr' package to bind the results into a dataframe.
#'  Resutls are presented as a table of proportions of wins and losses for each strategy
#'  rounded to two decimal places.
#' 
#' @param n The number of games to simulate.
#' 
#' @return 
#'  A dataframe containing the results of each strategy for every game iteration.  The 
#'  columns contain the strategy and the outcome of the game.  The dataframe contains 
#'  two rows for each game, one for the stay strategy and one for the #' switch strategy.
#' 
#' @examples
#' #'  play_game()
#'   - create_game() output: c("goat", "car", "goat")
#'   - select_door() output: 1 
#'   - open_goat_door( c("goat", "car", "goat"), 1) output: 3
#'   - change_door( stay=T, 3, 1) output: 1
#'    - determine_winner( 1, c("goat", "car", "goat")) output: "lose"
#'   - change_door( stay=F, 3, 1) output: 2
#'    - determine_winner( 1, c("goat", "car", "goat")) output: "win"
#' Output (n = 1):
#'   game.results("stay", "lose"
#'               "switch", "win")
#' 
#' Output (n = 3):
#'   game.results("stay", "lose"
#'               "switch", "win")
#'               "stay", "lose"
#'               "switch", "win"
#'               "stay", "win"
#'               "switch", "lose")
#' 
#' @export
#' 
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
