# save data to the server
cdds.save<- function(data.written.to.server, data.type, received.data = NULL, env = globalenv())
{
  # matrices or data.frames can be written only....
  if (is.matrix(received.data) || is.data.frame(received.data))
  {
     # convert received matrix to a data.frame, if required
     if(data.type == "data.frame")
     {
        data <- as.data.frame(received.data)
     }
     else
     {
       data <- received.data
     }

     # assign data to the environment
     if (data.type %in% class(data))
     {
       assign(data.written.to.server, data, envir = env)
     }
     else
     {
       stop("SERVER::ERR::SHARING::102")
     }

     return(exists(data.written.to.server, envir = env)
            & data.type %in% class(get(data.written.to.server, envir = env)))
  }
}

cdds.concat <- function(data.written.to.server, received.data, env = globalenv())
{
  concat.data <- received.data
  print("AAA")
  # check the R object has been created
  if(exists(data.written.to.server, envir = env))
  {
    print("BBB")
    data         <- get(data.written.to.server, envir = env)

    # check data type is either a matrix or a data frame
    if((is.data.frame(data) || is.matrix(data)) &
       (is.data.frame(received.data) || is.matrix(received.data)))
    {
      print("CCC")
      no.col.data     <- ncol(data)
      no.col.received <- ncol(received.data)

      print("DDD")
      if(no.col.data == no.col.received)
      {
        print("EEE")
        # concat data
        concat.data <- rbind(data, received.data)

      }
      else
      {
        stop("SERVER::ERR::SHARING::104")
      }
    }
    else
    {
      stop("SERVER::ERR::SHARING::103")
    }
  }
  return(concat.data)

}

# assign data to server variable
cdds.assign.data <-  function(data.written.to.server = "",
                              data.type              = "",
                              is.new.var = TRUE,
                              header = "",
                              payload = "",
                              property.a = 0,
                              property.b = 0,
                              property.c = 0.0,
                              property.d = 0.0)
{
  outcome  <- FALSE
  #get settings and set environment to global env
  env              <- globalenv()
  #settings         <- get.settings(envir = env)

  #convert payload into a matrix and save it as an R object
  received.data  <- adds.create.matrix(data = payload,no.columns = property.b)

  # concatenate  and save
  if(!is.new.var)
  {
    concat.data <- cdds.concat(data.written.to.server, received.data, env)
    # save data
    outcome <- cdds.save(data.written.to.server, data.type, concat.data, env)
  }
  else
  {
    # save data
    outcome <- cdds.save(data.written.to.server, data.type, received.data, env)
  }

  # returns TRUE if correctly saved. Otherwise false
  return(outcome)
}
#'@name concatDataToVariableDS
#'@title  concat data received from the client to the server
#'@description This server-side function add some rows to a matrix or a dataframe. If the R objects has yet
#'to be created, then it is created.
#'@param data.written.to.server character argument. name of the server-side R object
#'@param class.type charater.argument. Either a matrix or a data.frame
#'@param is.new.var logical argument. Set by default to TRUE. Replace existing object when set to TRUE.
#'When set to FALSE, concatenate data.
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
concatDataToVariableDS <- function(data.written.to.server = "",
                                   class.type             = "data.frame",
                                   is.new.var = TRUE,
                                   header = "",
                                   payload = "",
                                   property.a = 0,
                                   property.b = 0,
                                   property.c = 0.0,
                                   property.d = 0.0)
{

  if (is.sharing.allowed())
  {

    if ( is.character(header)   & is.character(payload)   &
         is.numeric(property.a) &  is.numeric(property.b) &
         is.numeric(property.c) & is.numeric(property.d)  &
         is.character(data.written.to.server)             &
         is.character(class.type)                         &
         is.logical(is.new.var))
    {
      if (nchar(header) > 0 & nchar(payload) > 0 & property.a > 0
          & property.b > 0 & property.c > 0 & property.d > 0)
      {

        return(cdds.assign.data(data.written.to.server, class.type, is.new.var, header, payload,property.a, property.b, property.c, property.d))
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
}
