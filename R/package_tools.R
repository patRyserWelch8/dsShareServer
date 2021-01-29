#'@name is.sharing.allowed
#'@title  verifies the variables used to set the parametrisation to for sharing parameters
#'exists on a DataSHIELD server.
#'@description This server function checks some settings used exchange parameters between
#'DataSHIELD server exists. It also verifies the data owners and governance have allowed the
#'sharing of parameters in a specific server.
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'
is.sharing.allowed <- function()
{
  outcome <- FALSE
  if (exists("settings",where = 1))
  {
    outcome <- settings$sharing.allowed
  }
  return(outcome)
}

#'@name encode.data.with.sharing
#'@title  encode some obscured  data to be exchanged from one server to another.
#'@description This server function can only be used with some encrypted data. It
#'format the data prior its transfer to a client-side function.
#'@param encrypted.data - data to be encoded
#'@param length - an indication how long should be the data
#'@param index - A random number related to the data
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@seealso \link[dsShareServer]{getDataDS}, \link[dsShareServer]{getCoordinatesDS},
#'\link[dsShareServer]{encode.data.no.sharing}
encode.data.with.sharing <- function(encrypted.data, length, index)
{
  #remove conversion once new parsers is available
  header        <- ""
  data          <- as.character(paste(as.numeric(encrypted.data),sep="",collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  timestamp     <- as.numeric(Sys.time()) / size

  return.value  <- list(header = "FM1" ,
                        payload = data,
                        property.a = size,
                        property.b = length,
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}

#'@name encode.data.no.sharing
#'@title  encode some randomised data
#'@description This server-side function generates some random data to be made available to a client-side
#'function. Its purpose is to mimick the same behaviour as [dsServerParameter]{encode.data.with.sharing}. It aims a
#'a "decoy", if an error has occurred in the process.
#'
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@seealso \link[dsShareServer]{getDataDS} \link[dsShareServer]{getCoordinatesDS},[dsShareServer]{encode.data.no.sharing}
encode.data.no.sharing <- function()
{
  header        <- ""
  data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- as.integer(stats::runif(1, min=settings$min_rows, max=settings$max_rows))
  no.rows       <- as.integer(stats::runif(1, min=settings$min_columns, max=settings$max_columns))
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)
  return(return.value)
}

#'@name are.params.created
#'@title check the some variables considered as parameters are created on a server
#'@param param_names names of params
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
are.params.created <- function(param_names = c())
{
  params.exist <- FALSE
  all.numeric  <- FALSE

  if (length(param_names) > 0)
  {
    if(length(param_names) >=1  & is.character(param_names) )
    {
      list.var      <- ls(pos = 1)
      params.exist  <- all(param_names %in% list.var)
      if(params.exist)
      {
        #get the object and check for numerical values. mget checks for the existence and
        #retrieve object.
        params      <-  mget(x = param_names, envir = as.environment(1))
        all.numeric <- all(sapply(params, is.numeric))
      }
    }
  }
  return(params.exist & all.numeric)
}

#'@name get.transfer
#'@title returns the transfer list if it is correctly setup
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@param settings some settings
#'@note  Throws error "SERVER::ERR:SHARE::013" if transfer is not created on server.
#'"Throws "SERVER::ERR:SHARE::014" if the transfer list is created on a server, with the incorrect field.
get.transfer <- function(settings)
{
  transfer <- NULL
  if(exists(settings$name.struct.transfer, where = 1))
  {
    transfer      <- get(settings$name.struct.transfer, pos = 1)
    correct.field <- settings$current_row %in% names(transfer)
    if(!correct.field)
    {
      transfer <- list()
      transfer[[settings$current_row]] = 1
    }
  }
  else
  {
    transfer <- list()
    transfer[[settings$current_row]] = 1
  }
  return(transfer)
}

#'@name are.encoded.data.and.settings.suitable
#'@title check some settings encoded data and settings are suitable for continuing transferring
#'@details This is a helper function. It cannot be called directly from any client-side
#'function.
#'@description  It checks the sharing for the following criteria:
#' 0. sharing is allowed
#' 1. the encoded data is the same as previously stated in the encoding check
#' 2. encoded data exists
#' 3. encoded data is a data frame
#' 4. the data encoded is character
#'@param data.encoded some encoded data
#'@note Throws the following errors:
#'"SERVER::ERR:SHARE::002"  sharing is not allowed or the disclosure setting has not been set to 0 or 1
#'"SERVER::ERR:SHARE::005"  data.encoded does not exists on the server
#'"SERVER::ERR:SHARE::008"  data.encoded is not the same R object as previously validated \code{isDataEncodedDS}
#'"SERVER::ERR:SHARE::009"  data.encoded has yet to be validated by \code{isDataEncodedDS}
#'"SERVER::ERR:SHARE::010"  data.encoded is not a character vector
#'
are.arg.and.settings.suitable <- function(data.encoded)
{
  outcome <- FALSE
  if(is.sharing.allowed())
  {
    settings  <- get("settings", pos = 1)

    if(!is.character(data.encoded))
    {
      stop("SERVER::ERR:SHARE::010")
    }

    same.name <- identical(settings$encoded.data.name,data.encoded)
    data.exists <- exists(data.encoded, where = 1)
    if(!data.exists)
    {
      stop("SERVER::ERR:SHARE::009")
    }

    correct.format <- is.data.frame(get(data.encoded, pos = 1))
    if(!correct.format)
    {
      stop("SERVER::ERR:SHARE::005")
    }

    same.name <- identical(settings$encoded.data.name,data.encoded)
    if(!same.name)
    {
      stop("SERVER::ERR:SHARE::008")
    }
    outcome <- same.name & data.exists & correct.format
  }
  else
  {
    stop("SERVER::ERR:SHARE::002")
  }
  return(outcome)
}
