

#'@name isEndOfDataDS
#'@title Verifies the end of some encoded data on the server has been reached
#'@description This server function indicates whether the number of rows transferred has exceeded
#'of equal to the number of rows in the encoded data frame.
#'@param data_encoded - character argument specifying the name of the encoded data on a data server
#'@return A boolean value. TRUE if the last row of the encoded data has been transferred.
#'Otherwise, returns FALSE
#'@seealso \link[dsShareServer]{nextDS}, \link[dsShareServer]{isDataEncodedDS}
#'@export
#'
isEndOfDataDS <- function(data_encoded = NULL)
{

  if (is.sharing.allowed())
  {
    outcome <- TRUE
    arg.and.settings.suitable <- are.arg.and.settings.suitable(data_encoded)
    if(arg.and.settings.suitable)
    {
      settings <- get("settings", pos = 1)
      transfer <- get.transfer(settings)
      outcome  <- transfer[[settings$current_row]] >= nrow(get(data_encoded,pos = 1))
    }
    return(outcome)
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }

}
