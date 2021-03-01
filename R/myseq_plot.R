#' Produce the plot by given data along using myseq_n() function
#'
#' @description
#' For hw04 function 2, produce the plot by given df.
#'
#' @param numdf a four col df
#'
#' @return a line plot of the output values for the different values of n
#' @export
#'
#' @examples
#' my_data <- tibble::tribble(
#' ~x, ~y, ~z, ~n,
#' 2,4,3,3,
#' 2,4,3,4,
#' 2,4,3,5,
#' 2,4,3,6,
#' 2,4,3,7,
#' 2,4,3,8,
#' 2,4,3,9,
#' 2,4,3,10,
#' 2,4,3,12)
#' myseq_plot(numdf = my_data)

myseq_plot <- function(numdf) {
  bkdf <- tibble(n = 0, output = 0) # blank df
  numdf <- tibble(numdf)
  for (i in 1:nrow(numdf)) {
    x <- c(numdf[[i, 1]], numdf[[i, 2]], numdf[[i, 3]]) # extract x, y, z (separate extract)
    n <- numdf[[i, 4]] # extract n
    myseq_n(x, n) -> bkdf[i, 2] # put "x" and "n" in the blank df
    n -> bkdf[i, 1] # put "x" and "n" in the blank df
  }

  indf[ ,2] <- round(bkdf[ ,2], digits = 3) # for ggtitle
  innerplot <- bkdf %>% # plot
    ggplot2::ggplot(mapping = ggplot2::aes(x = n, y = output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste("My Sequence:", bkdf[ ,2])) # print the given title

  return(innerplot)
}
