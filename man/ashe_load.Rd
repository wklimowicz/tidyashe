% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile.R
\name{ashe_load}
\alias{ashe_load}
\title{Load ASHE from DATA_DIRECTORY folder}
\usage{
ashe_load(set_key = 1, as.data.table = TRUE, ...)
}
\arguments{
\item{set_key}{Set key to YEAR/PIDEN?}

\item{as.data.table}{Passed to read_fst}

\item{...}{Passed to fst::read_fst}
}
\description{
To be able to use one file across many projects,
set an environment variable called \code{DATA_DIRECTORY} with a folder path,
and use this function to load the data in.
}
