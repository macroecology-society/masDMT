% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_data.R
\name{list_data}
\alias{list_data}
\title{Create a sandbox containing small subsets of target datasets.}
\usage{
list_data(data_id)
}
\arguments{
\item{data_id}{Element or vector of class \emph{character}.}
}
\value{
A \emph{data.frame}.
}
\description{
Create a sandbox containing small subsets of target datasets.
}
\details{
{When no argument is used, the function will return a
\emph{data.frame} with a summary of the existing datasets and
their general characteristics. When \emph{data.id} is provided,
the function will search for the corresponding dataset and return
a \emph{data.frame} reporting on the individual files that compose
each of the target datasets.}
}
