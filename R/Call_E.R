#' R FUCTION
#' @description
#' Call function according to the number of exercises requested.
#' You can review pdf <Exercises.pdf> with each process and lines of code.
#' @param x The number selection of the exercise (between 1 and 6).
#' @return The solution for the specified exercise.
#' @name Call_Exercises
#' @examples
#' Call_E(1)
#' Call_E(2)
#' ...
#' Call_E(6)
Call_E <- function(x)
 {
  if(x==1)
return(retrieve_answer(1))
  if (x==2)
return(retrieve_answer(2))
  if(x==3)
return(retrieve_answer(3))
  if(x==4)
return(retrieve_answer(4))
  if(x==5)
return(retrieve_answer(5))
  if(x==6)
return(retrieve_answer(6))
  if (x>=7 | x<=0){
return("Exercise no found")
  }
}
