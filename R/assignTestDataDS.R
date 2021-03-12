#'@name assignTestDataDS
#'@title assign datasets from testing the transfer of data
#'@Description assigns two datasets on the server for testing the transfer of data.
#'The server objects are named "encoded.data" and "mtcars".
#'@export
assignTestDataDS <- function()
{
  env <- globalenv()
  assign("encoded.data", as.data.fram(encoded.data), envir = env)
  assign("mtcars", mtcars, envir = env)
}
