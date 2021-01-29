

#'@name removeExchangeDataDS
#'@title Remove data used to exchange some parameters from a DataSHIELD server
#'@description This server function deletes the data used in the exchange of the parameters from a DataSHIELD server.
#'The settings and sharing data structure are deleted. This function is important, to keep temporary
#'data being analysed by other processes.
#'@return TRUE if the settings and sharing variables have been deleted. Otherwise, FALSE
#'@export
removeExchangeDataDS <- function()
{
  #ini variables. Error are thrown is some settings
  env           <- globalenv()
  settings      <- get.settings(envir = env)
  settings.name <- get.settings.name()
  sharing.name  <- get.sharing.name(envir = env)

  #remove sharing data
  if(exists(sharing.name, where = env))
  {
    remove(list = sharing.name, envir = env)
  }

  # remove settings
  if(exists(settings.name, envir = env))
  {
    remove(list = settings.name, envir = env)
  }

  return(!exists(sharing.name, envir = env) &
         !exists(settings.name, envir = env))
}
