#' Random Letter Generator
#' @param n the number of random letters you want to select
#' @param l_prob a vector of probabilities for each of the 26 letters (optional), default is 1/26
#' @param wreplacement if you want multiple letters, logical TRUE/FALSE for sampling with replacement
#' @return a random letter of probabilities
#' @export
#' @examples
#' randomletter(1) # select 1 random letter
#' randomletter(2) # select 2 random letters


randomLetter <- function (n, l_prob = rep(1/26, 26), wreplacement = FALSE) {
  iter <- sample(26, n, replace = wreplacement, prob = l_prob)
  letters[iter]
}
