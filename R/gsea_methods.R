#' @importFrom stats setNames p.adjust
named_sort = function(x, nm){
    sort(setNames(x, nm), decreasing = TRUE)
}


#' Run a GSEA by group
#'
#' @param formula a `formula` of the kind `numeric_score ~ group1 + group2 + ...` identifying the column containing the score vs the grouping variables, which will be interacted with each other
#' @param gene_identifier `character`
#' @param data `data.frame` in which the `formula` and `gene_identifer` are interpreted
#' @param set_list alternatively, a list of named and sorted input
#' @param ... passed to [fgsea::fgsea()]
#' @inheritParams fgsea::fgsea
#' @return an object of class `GroupedGSEA`, which is internally just a list [fgsea::fgsea()] objects.
#' @export
#'
#' @examples
#' data(exampleRanks, package = 'fgsea')
#' data(examplePathways, package = 'fgsea')
#' data = data.frame(score = exampleRanks, gene = names(exampleRanks), treatment = rep(c('A', 'B'), length.out = length(exampleRanks)))
#' gout = gsea_by_group(score ~ treatment, 'gene', data, pathways = examplePathways)
#' df = as.data.frame(gout)
#' plot(gout)
gsea_by_group = function(formula, gene_identifier, data, set_list, nperm = 500, maxSize = 500, ...){
    if( (!missing(formula) || !missing(data) || !missing(gene_identifier)) &&
       (!purrr::is_formula(formula) || !inherits(data, 'data.frame') || !is.character(gene_identifier))){
        stop("If one of `formula`, `data` or `gene_identifier` is provided, then all must be provided.")
    }

    if(!missing(formula)){
        rhs_formula = gsub("^.*~", "~", as.character(as.expression(formula)))
        lhs =  gsub("[ ]*~.*$", "", as.character(as.expression(formula)))
        if(!(lhs %in% names(data))) stop("`data` does not have column ", lhs)
        if(!(gene_identifier %in% names(data))) stop("`data` does not have column ", gene_identifier)
        set_list = plyr::dlply(.data = data, formula(rhs_formula),
                         .fun = function(df) {
                             named_sort(df[[lhs]], df[[gene_identifier]])
                         })
        }

    res = purrr::map(set_list, fgsea::fgsea, nperm = nperm, maxSize = maxSize, ...)
    class(res) = c('GroupedGSEA', class(res))
    res
}

globalVariables(c('cluster', 'pathway', 'NES', 'ES', 'min_rank', 'pval'))

#' @describeIn gsea_by_group coerce result to a `data.frame`
#' @param x object of class `GroupedGSEA`
#' @param topn report at least this many pathways (by minimum p value across groups)
#' @param wrap_len wrap pathway titles after this many characters
#' @param p.adjust_method passed to [stats::p.adjust()]
#' @param row.names ignored
#' @param optional ignored
#' @export
as.data.frame.GroupedGSEA = function(x, row.names = NULL, optional = TRUE, ..., topn = Inf, wrap_len = 30, p.adjust_method = 'BH'){
    if(length(ignored <- list(...))> 0) warning('Arguments ignored: ', names(ignored))
    '%>%' = dplyr::'%>%'
    gsea_groupdf = purrr::map_dfr(x, as.data.frame, .id = 'cluster')
    gsea_groupdf = gsea_groupdf %>% dplyr::mutate(p.adjust = stats::p.adjust(pval, method = p.adjust_method))
    gsea_groupdf = gsea_groupdf %>% dplyr::group_by(cluster) %>% dplyr::mutate(rank = rank(pval))
    gsea_groupdf = gsea_groupdf %>% dplyr::group_by(pathway) %>%
        dplyr::mutate(min_rank = min(rank),
                      NES = ifelse(!is.finite(NES), 0, NES),
                      signed_log10p = -log10(p.adjust)*sign(ES))

    gsea_groupdf = gsea_groupdf %>% dplyr::filter(min_rank <= topn)
    gsea_groupdf = dplyr::ungroup(gsea_groupdf) %>% dplyr::mutate(ID_name = stringr::str_wrap(pathway, wrap_len), cluster = factor(cluster))
    gsea_groupdf

}

#' @describeIn gsea_by_group make a plot of the results
#' @export
plot.GroupedGSEA = function(x, ..., topn = 5, wrap_len = 30, p.adjust_method = 'BH'){
    if(length(ignored <- list( ...))> 0) warning('Arguments ignored: ', names(ignored))

    if(!requireNamespace('ggplot2')) stop('Install ggplot2')
    y = as.data.frame(x, topn = topn, wrap_len = wrap_len, p.adjust_method = p.adjust_method)
    plt = ggplot2::ggplot(y, mapping = ggplot2::aes(y = cluster, x = NES, fill = cut(p.adjust, c(0, .01, .05, .1, .2,  1)))) + ggplot2::geom_point(shape = 21) + ggplot2::scale_fill_brewer('FDR', direction = -1, type = 'seq', palette = 'YlOrBr') + ggplot2::facet_wrap(~ID_name) + ggplot2::theme_minimal() + ggplot2::geom_vline(xintercept = 0, lty = 2)
    plt
}

filter_gsea = function(gsea_result, sets){
    stopifnot(length(setdiff(sets, names(gsea_result@geneSets)))==0)
    gsea_result@result = subset(gsea_result@result, ID %in% sets)
    gsea_result@geneSets =  gsea_result@geneSets[sets]
    gsea_result
}

