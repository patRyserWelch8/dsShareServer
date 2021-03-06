% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignVariable.R
\name{assignVariable}
\alias{assignVariable}
\title{assigns the name of R-server object to the settings used to transfer some encoded information}
\usage{
assignVariable(encoded.data = NULL, is.encoded.data = FALSE)
}
\arguments{
\item{encoded.data}{a character string with the name of the server R-server object used int the transfer}

\item{is.encoded.data}{a logical argument to indicate data has been encoded. Set to false by default}
}
\value{
TRUE
}
\description{
This server function sets a settings specific to transfer of encoded data; i.e.,
the name of the R object used.
}
