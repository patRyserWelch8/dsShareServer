# save matrix into global env as an R object as specified in settings
adds.save.matrix <- function(settings, received.matrix = NULL, master_mode, env = globalenv())
{
    if (is.matrix(received.matrix))
    {
      sharing                      <- get.sharing(envir = env)
      sharing[[settings$received]] <- received.matrix
      assign(get.sharing.name(), sharing, envir = env)
    }
}

# transform data in to a matrix
adds.create.matrix <- function(data = NULL,  no.columns = 1)
{
  # intialise matrix to be returned. If payload cannot be
  # decoded, then a 4 x 4 matrix made of 0 is returned.
  numbers         <- rep(x = 0, times=4)
  received.matrix <- matrix(as.numeric(numbers),2,2)

  # checks appropriate data type or classes.
  if (is.character(data) & is.numeric(no.columns))
  {
    # checks it can be converted to numerical values
    can.be.converted <- grepl('^-?[0-9.;e]+$', data)

    if(can.be.converted)
    {
      # split character string into a list of elements
      data.list       <- strsplit(data,";")
      # ???? length(data.list) - remove

      # check the split was suitable
      if (length(data.list[[1]]) > 1)
      {
          # transform into a vector and remove potential blank caracters
          data.vector <- unlist(data.list)
          data.vector <- gsub(" ", "",data.vector)

          # compute no rows
          no.rows     <- length(data.vector)/no.columns

          # check it is not a scalar!
          if (no.rows > 1 & no.columns > 1)
          {
              # transform vector as numeric values and then as a matrix
              data.numeric    <- as.numeric(x = data.vector)
              received.matrix <- matrix(data=data.numeric,nrow=no.rows, ncol= no.columns)
          }
      } # length
    } # converted
  }# arguments

  return(received.matrix)
}

# check matrix has been assigned.
adds.are.assigned.values.correct <- function(settings, master_mode, env = globalenv())
{
  outcome <- FALSE
  sharing <- get.sharing(envir = env)
  #if (exists(settings$name.struct.sharing,where=1))
  #{
   # sharing       <- get(settings$name.struct,pos=1)
    # set structure to verify
    structure     <- c(settings$received)

    # count number of entries in
    total.correct <- sum(structure %in% names(sharing))
    value.exists  <- length(structure) ==  total.correct

    # check class of matrix
    if (value.exists)
    {
      outcome <- is.matrix(sharing[[settings$received]])
    }
  #}
  return(outcome)
}

adds.assign.data <- function(master_mode = TRUE, header = "", payload = "", property.a = 0,
              property.b = 0, property.c = 0.0, property.d = 0.0)
{
  #get settings and set environment to global env
  env              <- globalenv()
  settings         <- get.settings(envir = env)

  #convert payload into a matrix and save it as an R object
  received.matrix  <- adds.create.matrix(data = payload,no.columns = property.b)
  adds.save.matrix(settings, received.matrix, master_mode, env)

  # returns TRUE if correctly saved. Otherwise false
  return(adds.are.assigned.values.correct(settings = settings, master_mode =  master_mode, env = env))
}

#'@name assignDataDS
#'@title  assign data to one or multiple servers with some encrypted data from the analysis computer
#'@description This server function assigns some values into a specific structure.
#'@param master_mode Boolean argument. It indicates the mode of a server is a \strong{master} or a \strong{receiver}. By default, set to TRUE.
#'@param header character argument. Header information received from another server.
#'@param payload  character argument. Payload information received from another server.
#'@param property.a numeric argument. Property.a received from another server.
#'@param property.b numeric argument. Property.a received from another server.
#'@param property.c numeric argument. Property.a received from another server.
#'@param property.d numeric argument. Property.a received from another server.
#'@details Some data are being assign into a specific structure used to share parameter in some privacy-protection settings. The process used by
#'\link[dsShareServer]{getDataDS} is reversed.
#'@seealso \link[dsShareServer]{getDataDS}
#'@export
assignDataDS <- function(master_mode = TRUE, header = "", payload = "", property.a = 0,
                              property.b = 0, property.c = 0.0, property.d = 0.0)
{

  if (is.sharing.allowed())
  {
    if ( is.character(header) & is.character(payload)
         & is.numeric(property.a) &  is.numeric(property.b)
         & is.numeric(property.c) & is.numeric(property.d))
    {
      if (nchar(header) > 0 & nchar(payload) > 0 & property.a > 0
          & property.b > 0 & property.c > 0 & property.d > 0)
      {

        return(adds.assign.data(master_mode,header, payload,property.a, property.b, property.c, property.d))
      }
      else
      {
        stop("SERVER::ERR::SHARING::006")
      }
    }
    else
    {
      stop("SERVER::ERR::SHARING::005")
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
}
