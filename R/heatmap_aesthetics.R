am_default_col = function (x, main_matrix = FALSE) {
    if (is.factor(x)) {
        x = as.vector(x)
    }
    if (length(unique(x)) == 1) {
        x = as.character(x)
    }
    attributes(x) = NULL
    x = x[!is.na(x)]
    if (is.character(x)) {
        levels = unique(x)
        if(length(levels)<=8){
            colors = RColorBrewer::brewer.pal(length(levels), 'Accent')
        } else{
            colors = hsv(runif(length(levels)), 1 - runif(1)/2,
                     1 - runif(1)/2)
        }
        names(colors) = levels
        return(colors)
    }
    else if (is.numeric(x)) {
        if (main_matrix) {
            if (length(unique(x)) > 100) {
                col_fun = circlize::colorRamp2(seq(quantile(x, 0.01),
                                         quantile(x, 0.99), length = 3), c("blue",
                                                                           "#EEEEEE", "red"))
            }
            else {
                col_fun = circlize::colorRamp2(seq(min(x), max(x), length = 3),
                                     c("blue", "#EEEEEE", "red"))
            }
        }
        else {
            col_fun = circlize::colorRamp2(range(min(x), max(x)), c("white",
                                                          hsv(runif(1), 1, 1)))
        }
        return(col_fun)
    }
}

#' Override the default color mappings in ComplexHeatmap
#' This function naughtily re-assigns a modified version of default_col into the ComplexHeatmap namespace
#' It will probably break in subsequent versions of complexheatmap.
#' @return none
#' @export
#' @importFrom grDevices hsv
#' @importFrom stats quantile runif
#' @importFrom utils assignInNamespace
set_AM_heatmap_defaults = function(){
    assignInNamespace('default_col', am_default_col, 'ComplexHeatmap')

}
