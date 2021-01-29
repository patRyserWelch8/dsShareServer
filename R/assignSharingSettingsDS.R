
if(getRversion() >= "3.1.0") utils::suppressForeignCheck("localvariable")


#'@name assignSharingSettingsDS
#'@title  assign the settings required to share parameters
#'@details The function assigns a variable in the global environment of a DataSHIELD server. A
#'list of parameters is used in the exchange of parameters. These parameters are to identified names
#'of values stored on the server temporarily during the exchange. Some other values are used to initialise
#'some matrices rows and columns.
#'@export
assignSharingSettingsDS <- function()
{
    settings <- list( sharing.allowed          = 0,
                      sharing.near.equal.limit = 1000,
                      encoded.data             = FALSE,
                      encoded.data.name        = "no_name",
                      name.struct.sharing      = "sharing",
                      name.struct.transfer     = "transfer",
                      current_row              = "current_row",
                      masking                  = "masking",
                      concealing               = "concealing",
                      received                 = "received",
                      encrypted                = "encrypted",
                      decrypted                = "decrypted",
                      data                     = "data",
                      index_x                  = "index_x",
                      index_y                  = "index_y",
                      param_names              = "param_names",
                      no_columns               = "no_columns",
                      no_rows                  = "no_rows",
                      min_rows                 = 11,#11,
                      max_rows                 = 21,#21,
                      min_columns              = 13,#13,
                      max_columns              = 23,#23,
                      min_value                = 1)



      if (!is.null(getOption("dsSS_sharing_param.name.struct")))
      {
        if(is.character(getOption("dsSS_sharing_param.name.struct")) &
           !identical(getOption("dsSS_sharing_param.name.struct"), ""))
        {
          settings$name.struct <- getOption("dsSS_sharing_param.name.struct")
        }
      }

    if (!is.null(getOption("dsSS_transfer.name.struct")))
    {
      if(is.character(getOption("dsSS_transfer.name.struct")) &
         !identical(getOption("dsSS_transfer.name.struct"), ""))
      {
        settings$name.struct.transfer <- getOption("dsSS_transfer.name.struct")
      }
    }

      if(!is.null(getOption("dsSS_sharing.allowed")))
      {
        if (getOption("dsSS_sharing.allowed") == 0)
        {
          settings$sharing.allowed <- FALSE
        }
        else if (getOption("dsSS_sharing.allowed") == 1)
        {
          settings$sharing.allowed <- TRUE
        }
        else
        {
          settings$sharing.allowed <- FALSE
        }
      }

      if(!is.null(getOption("dsSS_sharing.near.equal.limit")))
      {
        settings$sharing.near.equal.limit <- getOption("dsSS_sharing.near.equal.limit")
      }

      if(is.null(getOption("dsSS_settings"))) #settings name  have not been set as an option (see onload and description)
      {
        options(dsSS_settings = get.settings.name())
      }

      env <- globalenv()
      assign(getOption("dsSS_settings"),settings,envir = env)
      return(exists(getOption("dsSS_settings"),envir = env))
}



