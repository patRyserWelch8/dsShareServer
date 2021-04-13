#'@name assignDemoDataDS
#'@title  assign some data for testing sharing is working as expected
#'@details The function assigns two additional R small object in the global env of a
#'DataSHIELD server: (1) datashield.mtcars.data and (2) datashield.encrypted.data
#'@export
assignDemoDataDS <- function()
{
  env       <- globalenv()
  mtcars    <- dsShareServer::datashield_mtcars
  encrypted <- dsShareServer::encoded_data


  assign("datashield.mtcars.data", mtcars, envir = env)
  assign("datashield.encrypted.data", encrypted , envir = env)

  return(exists("datashield.mtcars.data", envir = env) &
         exists("datashield.encrypted.data", envir = env))
}
