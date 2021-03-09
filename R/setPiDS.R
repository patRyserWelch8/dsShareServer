#'@name setPiDS
#'@title set value for testing
#'@param newobj argument
#'@return logical value
#'@export
#only used for testing .... ds.connect.client
setPiDS <- function(newobj = "newobj")
{

   env     <- globalenv()
   assign(newobj, pi, envir = env)
   return(exists(newobj, envir = env))
}
