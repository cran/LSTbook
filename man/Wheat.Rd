% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wheat.R
\docType{data}
\name{Wheat}
\alias{Wheat}
\title{Experimental data on the yield of winter wheat}
\format{
A data.frame with 240 rows
\itemize{
\item genotype The type of wheat.
\item yield of the wheat from this plot
\item block Major region of the field
\item plot Subdivision of block in which the wheat was planted
\item year of the planting and measurement
}
}
\source{
Andrea Onofri \href{https://www.statforbiology.com/2023/stat_lmm_perennialcrops/}{"Repeated measures with perennial crops"}
}
\usage{
data("Wheat")
}
\description{
In the experiment, eight different varieties of winter wheat were planted in each of 7 calendar years (1996-2002).
Each genotype was assigned randomly to a \code{plot} within a \code{block}.
}
\keyword{datasets}
