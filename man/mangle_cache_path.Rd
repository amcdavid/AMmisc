% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knitr_functions.R
\name{mangle_cache_path}
\alias{mangle_cache_path}
\title{Mangle the knitr cache path so that it doesn't sync on Box}
\usage{
mangle_cache_path(cache_path = knitr::opts_chunk$get()$cache.path)
}
\arguments{
\item{cache_path}{initial cache path}
}
\value{
side-effect of altering knitr cache_path and the new path invisibly
}
\description{
Mangle the knitr cache path so that it doesn't sync on Box
}
