ndds.arg.correct <- function(data.encoded = NULL, no.rows = 1000)
{
  success                  <- FALSE
  arg.and.settings.suitable <- are.arg.and.settings.suitable(data.encoded)

  if(arg.and.settings.suitable)
  {
    is.correct.type         <- is.numeric(no.rows)
    if(is.correct.type)
    {
      is.positive <- no.rows > 0 # add here disclosure settings

      if(is.positive)
      {

          success             <- TRUE
      }
      else
      {
        stop("SERVER::ERR::SHARING::032")
      }
    }
    else
    {
      stop("SERVER::ERR::SHARING::031")
    }
  }
  else
  {
     stop("SERVER::ERR::SHARING::030")
  }
  return(success)
}

# This function encode a certain number of rows in a data frame.
ndds.encode.encoded.data <- function(settings, transfer, data.encoded, no.rows, env = globalenv())
{
  # get the settings, transfer data, and encoded data
  encoded      <- get(data.encoded, envir = env)
  transfer.data <- encode.data.no.sharing()


  #check current_row exists
  if (!(settings$current_row %in% names(transfer)))
  {
    transfer[[settings$current_row]] <- 1
  }

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

    if(start < max.rows)
    {

      unlist.data     <- unlist(encoded[c(start:end),])
      data            <- as.numeric(unlist.data)
      seed            <- generate.secure.seed(settings)
      set.seed(seed)
      index           <- stats::runif(1, min = .Machine$double.xmin, max  = .Machine$double.xmax)
      transfer.data   <- encode.data.with.sharing(data, ncol(encoded), index)


    }


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
#'@param data.encoded character argument representing the name of the encoded data
#'@param no.rows positive integer value indicating the number of rows to transfer
#'@return a list made of encoded data
#'@seealso \link[dsShareServer]{nextDS}, \link[dsShareServer]{isEndOfDataDS}
#'@export
nextDS <- function(data.encoded = NULL, no.rows = 1000)
{
  #check for sharing allowed by data owners. Error thrown if not allowed
  allowed     <- is.sharing.allowed()

  #check for errors. error thrown if not correct in function.
  args.correct <- ndds.arg.correct(data.encoded, no.rows)

  if(allowed & args.correct)
  {
    # sets function variables
    env                       <- globalenv()
    settings                  <- get.settings(envir = env)
    transfer                  <- get.transfer(envir = env)

    # prepare encoded data for transfer
      data.transfer <- ndds.encode.encoded.data(settings, transfer, data.encoded, no.rows, env)

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

}
