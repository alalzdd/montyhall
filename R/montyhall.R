#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
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
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Select a door from three doors
#' @description
#'   `select_door()` selects a door from the three different doors
#' @details
#'    build a new vector with 1-3 represents 3 doors and randomly
#'    return a number from the vector to represent a picked door.
#' @param ... no arguments are used by the function.
#' @return
#'    The function returns a random value in the door vector.
#' @examples
#'     select_door
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'    The host open a door with goat.
#' @description
#'    After the participants choosed a door, the host open another door
#'    with goat to help the participants.
#' @details
#'    If the participants picked the door with car behind, then host
#'    opened another door with goat. If the participants picked the
#'    door with goat behind, then host opened the other goat door.
#' @param
#' @return Return a door apart from the picked door and definitely
#'          with goat behind
#' @examples
#' @export
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
#'    Change or not change selected door.
#' @description
#'    The host offers participants a chance to change the selected
#'    door. Participants can choose change or not change.
#' @details
#'    Input a random number of stay value and return the changed
#'    door or the same door.
#' @param ... no arguments are used by the function
#' @return
#'    Return a final picked value of the door.
#' @examples
#' @export
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
#'    determine the winner
#' @description
#'    determine whether the participants win the game.
#' @details
#'    build a function to determine whether the participants picked
#'    a door with goat behind or car behind.
#' @param ... no arguments are used by the function
#' @return
#'    Return value WIN/LOSE.
#' @examples
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'     Testing code for the whole game.
#' @description
#'     Run every part of the game.
#' @details
#'     Run the whole game from crate game to report result.
#' @param ... no arguments are used by the function
#' @return
#'     Run every part of the game and return game result.
#' @examples
#' @export
play_game <- function( )
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
#'      Play the game n times and store the result.
#' @description
#'      Play the game multiple times.
#' @details
#'      Play the game multiple times and store the result.
#' @param ... no arguments are used by the function
#' @return
#'      Return a table of multiple game results.
#' @examples
#' @export
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
