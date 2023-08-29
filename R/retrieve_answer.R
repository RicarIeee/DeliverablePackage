#' R DATA ANALYSIS
#' @description
#' Solution of exercises to function "Call_E"
#' @param e_num The number of the exercise (between 1 and 6).
#' @return The solution for the specified exercise.
#' @name retrieve_answers
retrieve_answer <- function(e_num) {
  library(tidyverse)
  library(nycflights13)
  library(knitr)
  if (e_num == "1") {
    cat("\n 5.2.4 Exercises: items 1: Use the FILTER function to search for flights with an arrival delay of two hours or more.")
    P  <- nycflights13::flights
    P1  <- filter(P, arr_delay >="2")
    print(kable(P1[1:10, c(1, 14, 13, 9)],
          caption = "In this table you could see P1 information",
          align = "c"))
    cat("In the first point we made use of the filter function in order to obtain the data of the flights that had 2 or more hours of delay in arrival, for this we took the column arrive_delay and indicated that the data greater (>2) were displayed in the table above.\n")

    cat("\n 5.2.4 Exercises: items 2: I flew to Houston (IAH or HOU), I choose the IAH airport and filter it with the FILTER function.")
    P2 <- filter(P, dest =="IAH")
    print(kable(P2[1:10, c(1, 14, 13, 9)],
                caption = "In this table you could see P2 information",
                align = "c"))
    cat("In the previous point we wanted to show the data of the flights that landed in Houston (IAH or HOU). To develop this point we used the sign == which will allow us to take the data from the column with the word IAH.\n")
    return("Search other Exercise")
  }

  if (e_num == "2") {
    cat("5.3.1 Exercises: items 1: How could you use arrange() ans is.na")
    P3 <- flights %>% arrange(desc(is.na(dep_time)))
    print(kable(P3[1:10, c(1, 14, 13, 9)],
          caption = "In this table you could see P3 information",
          align = "c"))
    cat("For the previous exercise we used the arrange function to first organize or locate the data that was empty in the dep_time column.\n")

    cat("\n 5.3.1 Exercises: items 2: Sort the flights to find the most delayed flights. Find the flights that left the earliest. For the development of this item we used the data from the colimna (arr_delay), these were ordered from longest to shortest delay.")
    P4 <- flights %>% arrange(desc(arr_delay))
    print(kable(P4[1:10, c(1, 14, 13, 9)],
          caption = "In this table you can see P4 information",
          align = "c"))
    cat("In the previous exercise, the arrange function was used to organize the data of the flights that had the most delays in arriving at their destination. Taking into account the parameter desc. This will allow us to have the organization from the longest to the shortest.\n")

    cat("\n 5.3.1 Exercises: items 3: Sort the flights to find the fastest ones (higher speed).")
    P5 <- flights %>% mutate(speed = distance / air_time)%>% arrange((desc(speed)))
    print(kable(P5[1:10, c(1, 14, 13, 11, 20)],
          caption = "In this table you can see P5 information",
          align = "c"))
    cat("For the previous exercise we made use of the mutate function in order to add a new column that will contain the speed data of each flight, for this, we took the distance and the air time that the plane had, we applied the formula V = D/T, in this way we could determine which is the speed of each flight. Finally, the data was organized in such a way as to visualize first the flights that had the highest speed.\n")

    cat("\n 5.3.1 Exercises: items 4: Which are the farthest and the shortest flights?")
    P6 <- flights %>% arrange(desc(distance))
    P7 <- flights %>% arrange(distance)
    print(kable(P6[1:10, c(1, 14, 13, 16)],
          caption = "In this table you can see P6 information",
          align = "c"))
    print(kable(P7[1:10, c(1, 14, 13, 16)],
          caption = "In this table you can see P7 information",
          align = "c"))
    cat("In the previous point, we made use of the arrange function in order to organize the data of the distance column, taking into account the (desc) particle that allows us to organize the data from highest to lowest, identifying the flights that took the longest distance.\n")
    return("Search other Exercise")
  }

  if (e_num == "3") {
    cat("5.4.1 Exercises: items 2: What happens if you include the name of a variable multiple times in a select() call?")
    cat("\n Answer: The variable is consistently present multiple times within an outcome.\n")

    cat("\n 5.4.1 Exercises: items 3:What does the any_of() function do? Why might it be helpful in conjunction with this vector?")
    vars <- c("year", "month", "day", "dep_delay", "arr_delay")
    cat("\n Answer: The any_of() function is employed to choose specific columns from a data frame using a character vector of column names. This can be valuable for the mentioned line of code \n")

    cat("\n 5.4.1 Exercises: items 4: Does the result of running the following code surprise you? How do the select helpers deal whit case by default? How can you change that default?")
    print(kable(select(flights[1:20,], contains("TIME")),
                caption = "In this table you can see TIMES that contain",
                align = "c"))
    cat("Answer:
1.  The dataset is grouped by aircraft tail number (tailnum), allowing subsequent calculations to be done separately for each aircraft.
2.  The *summarize()* function is used to calculate summary statistics for each aircraft. Inside the *summarize()* function:
    -   *total_flights* is determined using the *n()* function, which counts the total number of flights for each aircraft.\n
    -   *punctual_flights* is calculated using the *sum()* function, counting flights where the arrival delay (arr_delay) is less than or equal to 0 (on-time or early arrivals). The argument *na.rm = TRUE* handles missing values in the arr_delay column.\n
    -   *punctuality_percentage* is computed as the ratio of punctual flights to total flights, multiplied by 100 to obtain the percentage.\n

3.  After summarization, the groups (aircraft) are sorted using the *arrange()* function based on their punctuality percentages in ascending order. This means aircraft with the lowest punctuality percentages will be listed first.
4.  The *filter()* function is employed to remove rows where the punctuality_percentage is not available (NA).
5.  The resulting dataset is stored in the variable *worst_punctuality*, which contains information such as aircraft tail numbers, total flights, punctual flights, and corresponding punctuality percentages.
6.  Finally, the *worst_punctuality* dataset is displayed, showing the tail numbers of aircraft with the lowest punctuality percentages.\n")
    return("Search other Exercise")
  }

    if (e_num=="4"){
    cat("5.5.2 Exercises: items 1: Currently \`dep_time\` and \`sched_dep_time\` are convenient to look at, but hard to compute with because they're not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.")
      P8 <- flights %>% mutate( dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100, sched_dep_time_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100)
      print(kable(P8[1:10, c(1, 14, 13, 9)],
            caption = "In this table you can see P8 information",
            align = "c"))
      cat("The mutate() function is employed to introduce two additional columns: dep_time_mins and sched_dep_time_mins. These columns indicate the departure time and scheduled departure time, both translated into minutes starting from midnight. The computation (dep_time %/% 100) * 60 + dep_time %% 100 transforms the given hour and minutes into a total count of minutes.\n")

      cat("\n 5.5.2 Exercises: items 2: Compare \`air_time\` with \`arr_time - dep_time\`. What do you expect to see? What do you see? What do you need to do to fix it?")
      P9 <- P8 %>% mutate(arr_dep_time_diff = arr_time - dep_time_mins) %>% filter(!is.na(air_time) & !is.na(arr_dep_time_diff)) %>% select(air_time, arr_dep_time_diff)
      print(kable(P9[1:10, c(1, 2)],
            caption = "In this table you can see P9 information",
            align = "c"))
      cat("Using the mutate() function, the time difference between arrival and departure is calculated in minutes (arr_time - dep_time_mins). Then, the filter() function is used to remove rows with missing values in air_time or the time difference. Finally, the select() function is applied to keep only two columns: air_time (flight duration) and arr_dep_time_diff (time gap between arrival and departure in minutes)")
      return("Search other Exercise")
    }

      if(e_num=="5"){
      cat("5.6.7 Exercises: item 1: Brainstorm at least 5 different ways to assess the typical characteristics of a group of fligh.")
      cat("\n This summary outlines different analyses related to flight delays: \n
1. Median Arrival Delay: Finding the central value representing the typical delay experienced upon arrival by calculating the median arrival delay for a group of flights.
2. Proportion of Flights with Specific Delays: Determining the percentage of flights arriving ei- ther significantly early or late (15 minutes, 30 minutes, or 2 hours). This provides insight into the distribution of delay scenarios within the group.
3. Average Departure Delay: Calculating the average delay before departure for a group of flights. This offers insight into the typical delay experienced before takeoff.
4. Punctuality Percentage: Calculating the percentage of flights that are punctual (no arrival delay) and comparing it to the percentage of flights significantly delayed (2 hours late). This contrasts on-time flights with extremely delayed ones.
5. Arrival Delay Distribution: Creating a histogram or density plot showcasing the distribution of arrival delays across all flights. This visualization identifies common delay ranges and outliers.\n")
      return("Search other Exercise")
      }

  if (e_num=="6"){
    cat("5.7.1 Exercises: item 2: Which plane (\`tailnum\`) has the worst on-time record?\n")
    cat("The dataset is categorized by aircraft tail number (tailnum). By employing the summarize() function, we compute summary statistics for every group (aircraft). Inside the summarize() function:
“total_flights” is calculated through the n() function, yielding the aggregate count of flights for each aircraft.
“punctual_flights” is derived using the sum() function, counting flights in which the arrival delay (arr_delay) is less than or equal to 0 (indicating punctual or early arrivals).
“punctuality_percentage” is computed as the proportion of punctual flights to total flights, then multiplied by 100 to express it as a percentage.\n")
    P10 <- flights %>% group_by(tailnum) %>% summarize(total_flights = n(),
    punctual_flights = sum(arr_delay <= 0, na.rm = TRUE),
    punctuality_percentage = (punctual_flights / total_flights) * 100) %>%
    arrange(punctuality_percentage) %>%
    filter(!is.na(punctuality_percentage))
    print(kable(P10[1:20, c(1,2,3,4)],
          caption = "In this table you can see P10.1 information",
          align = "c"))
    cat("Subsequent to the summarize() operation, we employ arrange() to arrange the groups (aircraft) based on their punctuality percentages in ascending sequence. This results in the aircraft with the lowest punctuality percentages being presented first. filter() is utilized to eliminate rows where the punctuality_percentage is unavailable (NA). The resultant dataset is assigned to the variable P10. \n")
    return("Search other Exercise")
  }
}

