
# verify the encoded parameters have been successfully encoded
dpds.is.encoded.param.valid <- function(settings, encoded.param = NULL)
{
  # sets variable for validation
  correct <- FALSE
  expected.list <- c(settings$encrypted,settings$masking,settings$received, settings$decrypted,
                     settings$index_x, settings$index_y)
  # verify class of encoded.param
  if (is.list(encoded.param))
  {
    # check list of attributes exists in sharing.
    list.attributes  <- names(encoded.param)
    attributes.exist <- list.attributes %in% expected.list
    total.correct    <- sum(attributes.exist == TRUE)

    # the oucome of the validation can become TRUE at this stage
    correct          <- (total.correct == length(expected.list))
  }
  return(correct)
}

# create a vector of names of parameters.  These names have not been
# transferred to the student. It is passed from the client call.
dpds.create.vector.params <- function (param_names = "")
{
  outcome    <- c()
  names.list <- strsplit(param_names,";")
  outcome    <- unlist(names.list)
  return(outcome)
}

dpds.decryptParam <- function(settings, sharing, param_names = NULL, tolerance = 8, env = globalenv())
{
  # sets varaible needed for the decryption process
  outcome       <- FALSE
  param.value   <- NA

  # transform "semi-column" separated names of parameters
  # into a vector
  params        <- dpds.create.vector.params(param_names)
  no.params     <- length(params)

  # obtain rows and columns coordinates for each parameter
  # each parameter has been set a index x and y. Decode
  # each coordinate.[see assignSettingsParamDS, getCoordinatesDS, assignCoordinatesDS]
  rows          <- ceiling(sharing[[settings$index_x]] * sharing[[settings$no_columns]])
  columns       <- ceiling(sharing[[settings$index_y]] * sharing[[settings$no_rows]])



  # check all the retrieve information are suitable for decryption
  rows.correct  <- all(rows    <= nrow(sharing$decrypted))
  cols.correct  <- all(columns <= ncol(sharing$decrypted))
  coord.correct <- length(rows) == length(params) & length(columns) == length(rows)


  if (rows.correct & cols.correct & coord.correct)
  {
      # retrieve each parametres using the coordinates. those are swapped due to transpose in encoding process

      for(param in 1:no.params)
      {
          param_name     <-  params[param]
          param.value    <-  round(sharing$decrypted[columns[param],rows[param]], tolerance)
          outcome[param] <- !is.na(param.value)
          assign(param_name,param.value, envir = env)
      }
  }

  return(all(outcome == TRUE))
}

#'@name decryptParamDS
#'@title  decrypt a server parameter
#'@description This server function decrypts a given parameter in matrix.
#'@param param_names character argument. Name of the variable used  to store the parameter value on a server.
#'@param tolerance numerical argument. Number of decimal places aimed to used for accuracy
#'@export
decryptParamDS <- function(param_names = NULL, tolerance = 8)
{
   outcome     <- FALSE
   param.value <- NA

   if (is.sharing.allowed())
   {
     # function variables
     env      <- globalenv()
     settings <- get.settings(envir = env)
     sharing  <- get.sharing(envir = env)

     # decrypt if previous steps have been successful
     if(dpds.is.encoded.param.valid(settings, encoded.param = sharing))
     {
       return(dpds.decryptParam(settings, sharing, param_names, tolerance, env))
     }
     else
     {
       stop("SERVER::ERR::SHARING::009")
     }
   }
   else
   {
     stop("SERVER::ERR::SHARING::001")
   }

}



