# This function encode a certain number of rows in a data frame.
ndds.encode.encoded.data <- function(settings, transfer, data.encoded, no.rows, env = globalenv())
{
  # get the settings, transfer data, and encoded data
  encoded      <- get(data.encoded, envir = env)
  tranfer.data <- encode.data.no.sharing()


  # calculate range of rows
  start     <- transfer[[settings$current_row]]
  max.rows  <- nrow(get(data.encoded, envir = env))
  diff.rows <- max.rows - start

  # check the current row has not exceeded or reach the end of a data frame
  if (start < max.rows & diff.rows > 0)
  {
    if(diff.rows >= no.rows)
    {
      end   <- start + no.rows
    }
    else
    {
      end   <- max.rows
    }

    # prepare for transfer
    names(encoded)  <- NULL
    data            <- as.numeric(unlist(encoded[c(start:end),]))
    seed            <- generate.secure.seed(settings)
    set.seed(seed)
    index           <- stats::runif(1, min = .Machine$double.xmin, max  = .Machine$double.xmax)
    transfer.data   <- encode.data.with.sharing(data, ncol(encoded), index)

    # update transfer
    transfer[[settings$current_row]] <- end
    assign(settings$name.struct.transfer, transfer, envir = env)
  }

  return(transfer.data)
}

#'@name nextDS
#'@title transfer a certain number of rows from some encoded data.
#'@description This server-side function transfer the next number of rows
#'to the client. Only suitably encoded data can be transferred.
#'@param data_encoded character argument representing the name of the encoded data
#'@param no.rows positive integer value indicating the number of rows to transfer
#'@return a list made of encoded data
#'@seealso \link[dsShareServer]{nextDS}, \link[dsShareServer]{isEndOfDataDS}
#'@export
nextDS <- function(data_encoded = NULL, no.rows = 1000)
{
  if(is.sharing.allowed())
  {
    # sets function variables
    env                       <- globalenv()
    settings                  <- get.settings(envir = env)
    transfer                  <- get.transfer(envir = env)
    arg.and.settings.suitable <- are.arg.and.settings.suitable(data_encoded)
    is.correct.type           <- is.numeric(no.rows)
    is.positive               <- FALSE

    # check correct rows representation
    if(is.correct.type)
    {
      is.positive <- no.rows > 0
    }

    # continue with process if all argument and settings are correct
    if(arg.and.settings.suitable & is.correct.type & is.positive)
    {
      # prepare encoded data for transfer
      data.transfer <- ndds.encode.encoded.data(settings, transfer, data_encoded, no.rows, env)

      # return to client data.transfer if suitably formatted
      if(identical(data.transfer$header, "FM2"))
      {
        stop("SERVER::ERR::SHARING::004")
      }
      else
      {
        return(data.transfer)
      }
    }
    else
    {
      stop("SERVER::ERR::SHARING::002")
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
}
