#'@name   assignVariableSettingsDS
#'@title  assigns the name of R-server object to the settings used to transfer some encoded information
#'@description This server function sets a settings specific to transfer of encoded data; i.e.,
#'the name of the R object used.
#'@param var_name  a character string with the name of the server R-server object used int the transfer
#'@return TRUE
#'@export
assignVariableSettingsDS <- function(var_name = NULL)
{
  if(is.sharing.allowed())
  {
    # check the argument value is numeric
    if (is.character(var_name))
    {
      # sets function variables
      env       <- globalenv()
      settings  <- get.settings(envir = env)
      transfer  <- get.transfer(envir = env)

      #check the R-server object exists.
      if(exists(var_name, where = env))
      {
         # save the name of of the R-server object
         settings$encoded.data.name <- var_name
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
           identical(settings$encoded.data.name, var_name))
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
}
