# check the option and return a suitable name.

# check the option and return a numerical value
asss.get.num.value <- function(current.value = 0, option.name = "")
{
  # new.value
  new.value <- current.value

  # obtain value
  # obtain the option value
  value     <- getOption(option.name)
  if (is.character(value) || is.numeric(value))
  {
    # check the contains some digits
    if (grepl("\\d", value))
    {
      value <- as.numeric(value)
      if(!is.na(value))
      {
        new.value <- value
      }
    }
  }
  return(new.value)
}

# check the option and return a boolean value
asss.get.logical.value <- function(current.value = 1, option.name = "")
{
  #set value. is.logical = true
  new.value  <- current.value
  value      <- as.integer(asss.get.num.value(current.value = as.numeric(current.value), option.name = option.name))
  if(value %in% c(0,1))
  {
     new.value  <- value
  }

  return(new.value)
}

#'@name assignSharingSettingsDS
#'@title  assign the settings required to share parameters
#'@details The function assigns a variable in the global environment of a DataSHIELD server. A
#'list of parameters is used in the exchange of parameters. These parameters are to identified names
#'of values stored on the server temporarily during the exchange. Some other values are used to initialise
#'some matrices rows and columns.
#'@export
assignSharingSettingsDS <- function()
{
    # create basic structure with default value
    settings <- list( sharing.allowed          = 1,
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
                      min_value                = 1,
                      footprint                = 9812735)


     # capture values from the Opal server
     settings$name.struct.sharing      <-  get.name(settings$name.struct.sharing,"dsSS_sharing_param.name.struct")
     settings$name.struct.transfer     <-  get.name(settings$name.struct.transfer,"dsSS_transfer.name.struct")
     settings$sharing.allowed          <-  asss.get.logical.value(as.logical(settings$sharing.allowed), "dsSS_sharing.allowed" )
     settings$sharing.near.equal.limit <-  asss.get.num.value(settings$sharing.near.equal.limit, "dsSS_sharing.near.equal.limit" )
     settings$footprint                <-  asss.get.num.value(settings$footprint,"dsSS_sharing.seed.footprint")

     # assign value in global env with the name of settings ....
     env <- globalenv()

     # get the name and store it in the environment
     setting.name   <- get.settings.name()
     assign(setting.name,settings,envir = env)

     # check it has been created and return outcome
     return(exists(setting.name,envir = env) & exists("dsSS") & is.sharing.allowed())
}



