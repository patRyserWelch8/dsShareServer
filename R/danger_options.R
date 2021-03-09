#'@name danger_options
#'@title retrieve the settings for the package
#'@description This function uses the option "dsSS_settings" and the global enviroment
#'to retrieve the settings
#'@export
danger_options <- function()
{
  return(.Options)
}
