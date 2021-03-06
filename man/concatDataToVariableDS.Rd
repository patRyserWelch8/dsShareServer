% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concatDataToVariableDS.R
\name{concatDataToVariableDS}
\alias{concatDataToVariableDS}
\title{concat data received from the client to the server}
\usage{
concatDataToVariableDS(
  data.written.to.server = "",
  class.type = "data.frame",
  is.new.var = TRUE,
  header = "",
  payload = "",
  property.a = 0,
  property.b = 0,
  property.c = 0,
  property.d = 0
)
}
\arguments{
\item{data.written.to.server}{character argument. name of the server-side R object}

\item{class.type}{charater.argument. Either a matrix or a data.frame}

\item{is.new.var}{logical argument. Set by default to TRUE. Replace existing object when set to TRUE.
When set to FALSE, concatenate data.}

\item{header}{character argument. Header information received from another server.}

\item{payload}{character argument. Payload information received from another server.}

\item{property.a}{numeric argument. Property.a received from another server.}

\item{property.b}{numeric argument. Property.a received from another server.}

\item{property.c}{numeric argument. Property.a received from another server.}

\item{property.d}{numeric argument. Property.a received from another server.}
}
\description{
This server-side function add some rows to a matrix or a dataframe. If the R objects has yet
to be created, then it is created.
}
\details{
Some data are being assign into a specific structure used to share parameter in some privacy-protection settings. The process used by
\link[dsShareServer]{getDataDS} is reversed.
}
\seealso{
\link[dsShareServer]{getDataDS}
}
