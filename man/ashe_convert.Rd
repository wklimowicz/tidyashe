% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile.R
\name{ashe_convert}
\alias{ashe_convert}
\title{Convert to fst for faster loading}
\usage{
ashe_convert(folder, new_folder, select_columns = NULL, incremental = TRUE)
}
\arguments{
\item{folder}{Folder with ASHE .sav files}

\item{new_folder}{Name of new folder for files to be converted to}

\item{select_columns}{Vector of columns to limit conversion}

\item{incremental}{Only convert files which don't exist in output directory}
}
\description{
Convert to fst for faster loading
}
