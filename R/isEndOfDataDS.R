#'@name isEndOfDataDS
#'@title Verifies the end of some encoded data on the server has been reached
#'@description This server function indicates whether the number of rows transferred has exceeded
#'of equal to the number of rows in the encoded data frame.
#'@param data.encoded - character argument specifying the name of the encoded data on a data server
#'@return A boolean value. TRUE if the last row of the encoded data has been transferred.
#'Otherwise, returns FALSE
#'@seealso \link[dsShareServer]{nextDS}, \link[dsShareServer]{isDataEncodedDS}
#'@export
#'
isEndOfDataDS <- function(data.encoded = NULL)
{
  # check sharing is allowed
  if (is.sharing.allowed())
  {
    # set return value. TRUE stop the exchange
    outcome <- TRUE

    # check that data exists and are encoded suitably ....
    arg.and.settings.suitable <- are.arg.and.settings.suitable(data.encoded)

    # check some number of rows are still available
    if(arg.and.settings.suitable)
    {
      # sets environment, settings and transfer information
      env      <- globalenv()
      settings <- get.settings(envir = env)
      transfer <- get.transfer(envir = env)

      # outcome becomes FALSE, if any rows remains untransferred.
      outcome  <- transfer[[settings$current_row]] >= nrow(get(data.encoded,envir = env))

    }

    return(outcome)
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }

}
