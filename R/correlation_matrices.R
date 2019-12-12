# Copied from psych
fisherz = function (rho)
{
  0.5 * log((1 + rho)/(1 - rho))
}

fisherz2r = function (z)
{
  (exp(2 * z) - 1)/(1 + exp(2 * z))
}


#' Generate a correlation matrix and confidence intervals
#'
#' Treatment of NAs is incomplete at this time (in particular, the degrees of freedom will not be correct).
#' @param x numeric matrix-like object
#' @param y numeric matrix-like object. If missing `y=x`.
#' @param conf.level numeric confidence level
#' @param ... passed to cor (eg `use = `)
#' @param method inference method, currently only 'fisher' is supported
#'
#' @return structure containing correlation matrix, `lr`, `ur` lower and upper CI, the pvalues from the associated tests `pz` and the degrees of freedom `dof`.
#' @export
#'
#' @examples
#' data(iris)
#' fc = fisher_corr(iris[, c('Sepal.Length', 'Sepal.Width')],
#' iris[, c('Petal.Length', 'Petal.Width')])
#' print(fc, include_pstar = TRUE)
#' print(fc, include_pstar = TRUE, include_ci = TRUE, cor_fmt = '%.1f')
fisher_corr = function(x, y, conf.level = .95, ..., method = 'fisher'){
  method = match.arg(method, 'fisher')
  if(missing(y)) y = x
  #if(!is.numeric(x) || !is.numeric(y)) stop("`x` and `y` must be numeric (matrices)")

  crosscor = cor(x,y, ...)
  fz = fisherz(crosscor)
  se = 1/sqrt(nrow(x)-3)
  qz = qnorm(1-(1-conf.level)/2)
  lr = fisherz2r(fz - qz*se)
  ur = fisherz2r(fz + qz*se)
  zz = fz/se
  pz = (1-pnorm(abs(zz)))*2
  out = structure(crosscor, lr = lr, ur = ur, pz = pz, dof = nrow(x) - 3, cor_call = match.call(cor, call('cor', x=x, y=y), expand.dots = TRUE))
  class(out) = 'AMcorr'
  out
}

get_pstar = function(pvals, pstar_levels){
  pstar_levels = sort(pstar_levels, decreasing = TRUE)
  out = rep('', length(pvals))
  for(i in seq_along(pstar_levels)){
    out[pvals< pstar_levels[i]]  = names(pstar_levels)[i]
  }
  out
}


#' @param x `AMcorr`
#' @param cor_fmt an sprintf format string for correlation
#' @param sep separators between correlation and confidence intervals, and/or pvalues
#' @param include_ci include upper/lower CI?
#' @param include_pstar include pvalue asterix (set according to `pstar_levels`)
#' @param pstar_levels named numeric vector determining pvalue formatting
#' @param ... ignored
#'
#' @return character matrix
#' @export
#' @describeIn fisher_corr  Pretty print a correlation matrix and optionally its confidence intervals and p values
#' @importFrom stats cor pnorm qnorm
print.AMcorr = function(x, cor_fmt = '%.2f', sep = ' ', include_ci = FALSE, include_pstar = FALSE, pstar_levels = c('*' = .05, '**' = .01), ...){
  sprintf_args = list(x = x)
  format_string = cor_fmt
  if(include_ci){
    format_string = paste(rep(cor_fmt, 3), collapse = sep)
    sprintf_args = c(list(attr(x, 'lr')),sprintf_args, list(attr(x, 'ur')))
  }
  if (include_pstar){
    format_string = stringr::str_c(format_string, ' ', '%s')
    pstar = get_pstar(attr(x, 'pz'), pstar_levels)
    sprintf_args = c(sprintf_args, list(pstar))
  }
  out = do.call(sprintf, c(list(fmt = format_string), sprintf_args))
  dim(out) = dim(x)
  dimnames(out) = dimnames(x)
  out
}
