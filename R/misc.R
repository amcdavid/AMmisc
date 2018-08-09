#' A quick and dirty heatmap
#'
#' @param mat numeric matrix
#' @param Rowv determines if and how the row dendrogram should be reordered.	By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a dendrogram, then it is used "as-is", ie without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	Has the options as the Rowv argument above and additionally when x is a square matrix, Colv="Rowv" means that columns should be treated identically to the rows.
#' @param symbreaks Boolean indicating whether breaks should be made symmetric about 0. Defaults to TRUE if the data includes negative values, and to FALSE otherwise.
#' @param col colors used for the image.
#' @param ... passed to gplots::heatmap.2
#' @return make a plot
#' @export
heat2 <- function(mat, Rowv=NA, Colv=NA, symbreaks=TRUE, col=if(symbreaks) gplots::redblue else gplots::redgreen,  ...){
    mat <- as.matrix(mat)
    gplots::heatmap.2(mat, trace='none', scale='none', Rowv=Rowv, Colv=Colv, symbreaks=symbreaks, col=col, ...)
}

lkup <- lookup::lookup

#' Clamp a vector between some modulus
#'
#' Replaces all values greater than modulus with modulus and all values lt modulus with -modulus.
#' Also known as winsorization.
#' @param x numeric vector
#' @param modulus numeric scalar
#'
#' @return numeric vector
#' @export
#'
clamp = function(x, modulus = 5){
    x[x < -modulus] = -modulus
    x[x> modulus] = modulus
    x
}

# add code to strip covariate columns from a data frame and convert to a numeric matrix
# add code to standardize ggplot themes

#' Largest elements in norm from a matrix
#'
#' The \code{top} largest rows in norm from \code{x} are retained.
#' Others are set to zero or removed (\code{remove_zero})
#' @param x numeric matrix
#' @param top number of top elements
#' @param remove_zero \code{logical}
#' @param p Coefficient `p` of the L_p norm
purify = function(x, top = floor(.1*nrow(x)), remove_zero = FALSE, p = 2){
    row_norm = rowSums(x^p)^(1/p)
    row_rank = rank(-row_norm, na.last = FALSE, ties.method = 'first')
    if(remove_zero){
    x = x[row_rank <= top,]
    } else{
     x[row_rank <= top,] = 0
    }
    return(x)
}
