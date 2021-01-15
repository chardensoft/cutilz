#' @name randomLetter
#' @title Random Letter Generator
#' @param n the number of random letters you want to select
#' @param l_prob a vector of probabilities for each of the 26 letters (optional), default is 1/26
#' @param wreplacement if you want multiple letters, logical TRUE/FALSE for sampling with replacement
#' @return a random letter of probabilities
#' @export
#' @examples
#' randomLetter(1) # select 1 random letter
#' randomLetter(2) # select 2 random letters

randomLetter <- function (n, l_prob = rep(1/26, 26), wreplacement = FALSE) {
  iter <- sample(26, n, replace = wreplacement, prob = l_prob)
  letters[iter]
}

#' @name cu.hist
#' @title Histogram Plotting Function
#' @usage cu.hist(d, bw = 0, highl = FALSE)
#' @param d the vector of data you want a histogram for
#' @param bw optional: set binwidth, otherwise it uses the default from ggplot
#' @param highl optional: sets breaks using seq, otherwise uses default
#' @return creates and prints a histogram
#' @export
#' @examples
#' cu.hist(d = c(1,1,1,1,2,2,3,3,3,3,3)) # plots a nice histogram of x
#' cu.hist(d = c(1,1,1,1,2,2,3,3,3,3,3), bw = 200) #plots a histogram of x with binwidth = 200
#' cu.hist(d = c(1,1,1,1,2,2,3,3,3,3,3), bw = 200, highl = TRUE) #plots a histogram of x with binwidth 200 with the most frequent bin highlighted.

cu.hist <- function (d, bw = 0, highl = FALSE) {
  if (bw == 0) {
    if (highl == TRUE) {
      print('If \"highl\" is true, bw needs a value other than 0.')
    }
    ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(x = d), color = "white", fill = "blue")
  } else if (highl == TRUE) {
    # d <- d
    # bw <- 2
    # brk <- seq(0, 8, 2)
    if (bw >= max(d)) {
      print("Warning: bw is greater than or equal to the data's max value, may cause errors.")
    }
    cut <- data.frame(table(cut(x = d, breaks = seq(0, ceiling(max(d)/bw)*bw, bw))))
    ggplot2::ggplot() +
      ggplot2::geom_histogram(
        ggplot2::aes(x = d),
        binwidth = bw,
        breaks = seq(0, ceiling(max(d)/bw)*bw, bw),
        color = "white",
        #For the group with maximum value, assign red, otherwise assign black
        fill = replace(rep("blue", NROW(cut)), which.max(cut$Freq), "red"))
  } else {
    ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(x = d),
                     color = "white", fill = "blue",
                     binwidth = bw)
  }
}

#' @name disneyCost
#' @title Calculate the cost of a Disney Trip
#' @param numPeople number of people going on the trip over age 2
#' @param numDays number of days on the trip
#' @param flightCost cost of a round trip ticket for one person
#' @param inDisney logical indicating whether you will be staying inside the Disney park hotels
#' @param daySpending your daily spending limit on souvenirs, etc.
#' @param mealsInPark how many meals you will purchase in the park vs. bringing your own food
#' @param costOfHomeMeal defaults to 8 dollars, if you anticipate a homemade meal to cost more per person you can adjust this
#' @param totalMeals defaults to 3 meals a day, if you plan to eat more meals a day than 3, you can adjust this
#' @param costOfParkMeal defaults to 25 dollars, if you anticipate spending more than 25 per person on a meal in the park you can adjust this
#' @param inParkHotel defaults to 500 dollars, if you anticipate spending more per night for your in-park hotel, you can adjust this
#' @param outParkHotel defaults to 100 dollars, if you anticipate spending more per night for your out-of-park hotel, you can adjust this
#' @return a dollar amount for the cost of the trip
#' @export
#' @examples
#' disneyCost(2, 3, 300, F, 50, 2) # returns the cost of a 3-day trip for 2 adults eating 2 meals in the park, spending $50/day and with a $300 flight per person.
#'

disneyCost <- function(numAdults, numDays, flightCost, inDisney, daySpending, mealsInPark,
                       costOfHomeMeal = 8, totalMeals = 3, costOfParkMeal = 25, inParkHotel = 500,
                       outParkHotel = 100) {
  if (numDays == 1) {
    tick <- 109
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 2) {
    tick <- 107
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 3) {
    tick <- 105
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 4) {
    tick <- 103
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 5) {
    tick <- 88
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 6) {
    tick <- 75
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 7) {
    tick <- 67
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 8) {
    tick <- 61
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 9) {
    tick <- 56
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  } else if (numDays == 10) {
    tick <- 52
    if (inDisney) {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              inParkHotel+daySpending)+numAdults*flightCost
    } else {
      totalCost <- numDays*(numAdults*(tick+(costOfHomeMeal*(totalMeals-mealsInPark))+(costOfParkMeal*mealsInPark))+
                              outParkHotel+daySpending)+numAdults*flightCost
    }
  }
  totalCost
}

#' @name nbaPlot
#' @title Outputs an nba court as a plot
#' @import ggplot2
#' @return graph showing nba court
#' @export
#' @examples
#' nbaPlot() # shows an NBA court in the Plots window

nbaPlot <- function() {
  ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  ###outside box:
  geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,47,47,-47,-47)))+
  ###halfcourt line:
  geom_path(data=data.frame(x=c(-25,25),y=c(0,0)))+
  ###halfcourt semicircle:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###solid FT semicircle above FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###dashed FT semicircle below FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  ###key:
  geom_path(data=data.frame(x=c(-8,-8,8,8,-8),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47)))+
  ###box inside the key:
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)))+
  ###restricted area semicircle:
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  ###rim:
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  ###backboard:
  geom_path(data=data.frame(x=c(-3,3),y=c(43,43)),lineend='butt')+
  geom_path(data=data.frame(x=c(-3,3),y=-c(43,43)),lineend='butt')+
  ###three-point line:
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  ###fix aspect ratio to 1:1
  coord_fixed()
}









