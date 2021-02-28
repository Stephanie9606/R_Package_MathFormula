#' Get the value of the formula
#'
#' @description
#' For hw04 function 1, get the answer from the given formula.
#'
#' @param x numeric
#' @param n integer which is >0
#'
#' @return the answer of the recursive sequence
#' @export
#'
#' @examples
myseq_n <- function(x, n) {
  stopifnot(length(x) == 3 & is.numeric(x)) # "x" error checking: correct length and type
  stopifnot(n > 0) # "n" error checking: type and is greater than 0
  numvec <- vector(mode = "integer", length = n)
  for (i in seq_along(numvec)) {
    if (i <= 3) {
      numvec[i] <- x[i]
    }
    else {
      numvec[[i]] <- numvec[[i-1]] + (numvec[[i-3]] - numvec[[i-2]])/i
    }
  }
  return(numvec[n])
}
# test
myseq_n(x = c(2, 3, 3), n = 3)
myseq_n(x = c(2, 4, 3), n = 4)
myseq_n(x = c(2, 4, 3), n = 5)
myseq_n(x = c(2, 4, 3), n = 6)
myseq_n(x = c(2, 4, 3), n = 7)
