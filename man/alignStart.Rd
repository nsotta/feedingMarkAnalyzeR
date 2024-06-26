% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_imagej_results.R
\name{alignStart}
\alias{alignStart}
\title{Align leaf area decrease curve at t=0}
\usage{
alignStart(leaf_area)
}
\arguments{
\item{leaf_area}{data.table generated by `makeLeafAreaTable`.}
}
\value{
data.table similar to the input `leaf_area` but LeafArea starts from 0 at frame = 1.
}
\description{
Align leaf area decrease curve at t=0
}
