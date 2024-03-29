% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_data.R
\name{register_data}
\alias{register_data}
\title{Build unique file identifier}
\usage{
register_data(input, output, overwrite = FALSE)
}
\arguments{
\item{input}{Path to the file to be added to the database.}

\item{output}{Name of the output file, following the output of \code{\link{build_id}}.}

\item{overwrite}{Logical. Should the function overwrite existing files?}
}
\value{
Writes file into standardized file structure.
}
\description{
Build unique file identifier
}
\details{
{This function helps users add new data file to a database as
created with \code{masDMT}. Note that the function will work with chmod to
modify file permissions. By default, the function will apply a
\emph{'read-only'} restriction (code 444), preventing potentially wrongful
overwrites or deletions. When \emph{overwrite} is set to TRUE, the function
will change the restrictions of a file to \emph{'read-write'}, allowing
users its modification. If this function is run in windows system, the
file will also \href{https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/icacls}{
receive a system-specific icacls}.}
}
\seealso{
\code{\link{build_id}} \code{\link{lockpick}}
}
