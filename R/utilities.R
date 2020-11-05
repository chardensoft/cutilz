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

