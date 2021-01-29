.get.transfer <- function(settings)
{
  transfer <- NULL
  if(exists(settings$name.struct.transfer, where = 1))
  {
    transfer <- get(settings$name.struct.transfer, pos = 1)
  }
  else
  {
    transfer <- list()
  }
  return(transfer)
}


assignTransfertSettingsDS <- function(current.row = NULL)
{
  if(is.sharing.allowed())
  {
    settings <- get("settings" , pos = 1)
    if(is.numeric(current.row))
    {
      transfer <- .get.transfer(settings)
      transfer[[settings$current_row]] <- current.row
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
}
