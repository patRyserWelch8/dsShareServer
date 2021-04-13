#'@name   assignVariable
#'@title  assigns the name of R-server object to the settings used to transfer some encoded information
#'@description This server function sets a settings specific to transfer of encoded data; i.e.,
#'the name of the R object used.
#'@param encoded.data  a character string with the name of the server R-server object used int the transfer
#'@param is.encoded.data a logical argument to indicate data has been encoded. Set to false by default
#'@return TRUE
assignVariable <- function(encoded.data = NULL, is.encoded.data = FALSE)
{
  if(is.sharing.allowed())
  {
    # check the argument value is numeric
    if (is.character(encoded.data) & is.logical(is.encoded.data))
    {
      # sets function variables
      env       <- globalenv()
      settings  <- get.settings(envir = env)
      transfer  <- get.transfer(envir = env)


      #check the R-server object exists.
      if(exists(encoded.data, where = env))
      {
         # save the name of of the R-server object
         settings$encoded.data.name <- encoded.data
         settings$encoded.data      <- is.encoded.data
         assign(get.settings.name(),settings, envir = env)


         # save the current_row to the transfer object
         transfer[[settings$current_row]] <- 1
         assign(get.transfer.name(),transfer, envir = env)
      }
      else
      {
        stop("SERVER::ERR:SHARE::009")
      }
    }
    else
    {
      stop("SERVER::ERR::SHARING::022")
    }
    return(any("encoded.data.name" %in% names(settings)) &
           identical(settings$encoded.data.name, encoded.data))
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
}
