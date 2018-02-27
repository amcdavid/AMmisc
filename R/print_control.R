#' Examine the upper left corner of a matrix-like object
#'
#' @param x a matrix or data.frame
#' @param p number of rows/columns to examine
#'
#' @return a slice of x
#' @export
#'
h2 <- function(x, p=15){
    if(!is.null(dim(x)) && dim(x)>1){
    n <- seq_len(min(p, nrow(x)))
    m <- seq_len(min(p, ncol(x)))
    x[n, m]
    } else{
        utils::head(x, n = p)
    }
}

#' Print everything
#'
#' @param x object to print
#' @param max.print maximum number of rows
#'
#' @return side effect of printing
#' @export
more <- function(x, max.print=9999){
    opar <- options(max.print=max.print)
    on.exit(options(opar))
    print(x, max=max.print)
}
