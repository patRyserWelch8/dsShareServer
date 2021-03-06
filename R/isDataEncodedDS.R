# Boolean function that checks whether the arguments of the functions are suitable
# for the comparison. Check the type and server variables have been created as data frame. Tibbles wrappes data frames.
idds.are.params.correct <- function(data.server = NULL, data.encoded = NULL,  env = globalenv())
{
  outcome <- FALSE
  if(is.null(.Options$dsSS_sharing.near.equal.limit) || is.null(.Options$dsSS_sharing.allowed))
  {
    stop("SERVER::ERR:SHARE::003")
  }

  if(is.character(data.server) &
     is.character(data.encoded))
  {
    # format as a vector as a vector of characters
    server.variables <- create.vector(data.server)
    indices          <- grep(pattern= "\\$",server.variables)

    if(!identical(indices, integer(0)))
    {
      variables        <- unlist(strsplit(server.variables[indices],"\\$"))
      variables        <- levels(factor(variables[seq_along(variables) %% 2 > 0]))
      server.variables <- c(variables, server.variables[!seq_along(server.variables) %in% indices])
    }

    # check all the server variables exists
    variables.created <- sapply(server.variables, function(name, env){exists(name, where = env)},env = env)
    variables.created <- all(TRUE == variables.created)

    # check data.encoded.exists
    encoded.created   <- exists(data.encoded, where = env)

    if(variables.created &
       encoded.created)
    {

       encoded        <- get(data.encoded, pos = env)


       if (!is.data.frame(encoded))
       {
         stop("SERVER::ERR:SHARE::005")
       }

       correct.format <- sapply(server.variables,
                                function(var){data.server    <- get(var, pos = env);
                                              return( is.data.frame(data.server) ||
                                                               is.list(data.server) ||
                                                               is.matrix(data.server) ||
                                                               length(data.server) > 1)})
       print(correct.format)

       correct.format <- all(TRUE == correct.format)
       print("UUUUUUUUU")
       print(correct.format)

       if(!correct.format)
       {
         stop("SERVER::ERR:SHARE::007")
       }


       outcome        <- correct.format &
                         is.data.frame(encoded)


    }
    else
    {
      stop("SERVER::ERR:SHARE::040")
    }
  }
  else
  {
    stop("SERVER::ERR:SHARE::011")
  }
  return(outcome)
}

# This helper function checks the two datasets are significantly the same, which is undesirable.
# It returns TRUE if it is signifincatly the same, and false if it is not.
idds.are.significant.same <- function(server, encoded)
{
  outcome <- FALSE
  if (is.numeric(server) & is.numeric(encoded))
  {
    if(all(is.na(server)) || all(is.na(encoded)))
    {
      outcome <- FALSE
    }
    else
    {
      t       <- stats::t.test(server, encoded, conf.level = 0.99, na.action=stats::na.omit)
      mann    <- stats::wilcox.test(server, encoded, conf.level = 0.99, exact=FALSE)
      outcome <- t$p.value >= 0.01 || mann$p.value >= 0.01
    }
  }
  else
  {
    stop("SERVER:ERR::SHARE::004")
  }
  return(outcome)


}

# check the limits sets in the  opal server  are preserved through through both datasets
idds.are.values.in.limit <- function(server, encoded, limit)
{
   if(is.numeric(server) & is.numeric(encoded))
   {
     diff <- abs(summary(server) - summary(encoded))
     return(all(diff <= limit))
   }
   else
   {
     return(FALSE)
   }
}

# This helper function returns a value between 1 and 6 if one of the check has failed. Otherwise, 7 if the data is sufficiently
# encoded.
# check no 1: identical - same R objects
# check no 2: some values from the servers are present in the encoded values
# chekc no 3: some values are not numeric
# check no 4: two datasets are significantly the same (t.test and mann.whitney, confidence level 0.99)
# check no 5: encoded values are still in the limits sets by the data governance
idds.is.encoded <- function(server, encoded, limit)
{
  #init function variables
  step      <- 0
  max       <- 5
  is.failed <- FALSE
  continue  <- TRUE

  # convert into vectors data passed
  server.data     <- idds.convert.data(server)
  encoded.data    <- idds.convert.data(encoded)

  # check encoding
  while (continue)
  {
    is.failed <- switch(step + 1,
                        identical(server.data, encoded.data), # 1 identical variables
                        any(server.data %in% encoded.data), #2 some values are present in both datasets
                        !is.numeric(encoded.data), #3 has some non-numeric values
                        idds.are.significant.same(server.data, encoded.data), # 4 data are significantly the same at the point of centrality
                        idds.are.values.in.limit(server.data, encoded.data, limit)) #5 data are with the limit min, max, mean, median, IQR

    step     <- step + 1
    continue <- !is.failed & step < max
  }

  #!is.failed add 1, when  is.failed is false. Otherwise 0, when it is TRUE
  return(step + !is.failed)
}

# This function checks the server variable is encoded suitably.
idds.check.encoding.variable <- function(server,encoded, limit)
{
  outcome <- FALSE

  if (is.data.frame(server))
  {
    outcome <- idds.check.encoding.data.frames(server, encoded, limit)
  }
  else if (is.list(server) || is.matrix(server) || is.vector(server))
  {
      no_steps <- idds.is.encoded(server,encoded,limit)
      outcome  <-  (no_steps == 6)
  }
  return(outcome)
}

# This function checks the a data frame is suitable encoded every column of a server is
# checked after each column of the server dataframe.
idds.check.encoding.data.frames <- function(server, encoded, limit)
{
  is.encoded      <- TRUE
  classes_server  <- lapply(server,class)
  classes_encoded <- lapply(encoded, class)

  for(i in 2:ncol(encoded))
  {
      for(j in 2:ncol(server))
      {
         if(grepl(classes_encoded[[i]], classes_server[[j]])  )
         {
            no_steps <- idds.is.encoded(server[j],encoded[i],limit)
            if(no_steps < 6)
            {
              is.encoded <- FALSE
              break
            }
          }
      }
  }
  return(is.encoded)
}

# converts data into a vector of numbers.
idds.convert.data <- function(data)
{

  if (is.data.frame(data))
  {
    #if data frame has some mixed data types the data may not be converted as expected. i.e; character becomes numerical values
    matrix.conversion <- data.matrix(data, rownames.force = NA)
    colnames(matrix.conversion) <- NULL
    outcome <- c(matrix.conversion)
  }
  else if (is.list(data))
  {
    outcome <- unlist(data)
  }
  else if(is.matrix(data))
  {
    outcome <- c(data)
  }
  else
  {
    outcome <- data
  }

  return(outcome)
}

# check number of columns is greater for the encoded data.
idds.check.dimension <- function(server, encoded)
{
  outcome <- FALSE
  if (is.list(server))
  {
    lengths <- unlist(lapply(server,length))
    outcome <- ncol(encoded) > 1 & all(lengths == nrow(encoded))
  }
  else if(is.vector(server))
  {
    outcome <- ncol(encoded) > 1 & length(server) == nrow(encoded)
  }
  else if (is.data.frame(server))
  {
    outcome <- ncol(encoded) > ncol(server) & nrow(encoded) == nrow(server)

  }

  return(outcome)

}

#this function assign the setting "encoded.data" to the results of the checks
# redundant instead use assignVariable
idds.set.settings <- function(outcome = FALSE, data.encoded)
{
  if(exists(get.settings.name(), where = 1))
  {
    env                        <- globalenv()
    settings                   <- get.settings(envir = env)
    settings$encoded.data      <- outcome
    settings$encoded.data.name <- data.encoded
    assign(get.settings.name(), settings, envir = env)
  }
}


idds.check.variables <- function(name.var, encoded, settings, env)
{
  outcome <- FALSE

  # split to obtain name of data frame or lists
  data.server.split    <- unlist(strsplit(name.var,"\\$"))

  # get the data from the servers and limit for comparisons
  server               <- get(data.server.split[1],  envir = env)
  limit                <- settings$sharing.near.equal.limit

  # complete checks
  if(idds.check.dimension(server, encoded))
  {
    outcome <- idds.check.encoding.variable(server, encoded, limit)
  }

  return(outcome)
}

#'@name isDataEncodedDS
#'@title check some R objects are suitably encoded
#'@details This server function verifies the following rules are applied to the encoded data
#'against (1) a server variable and (2) a datasets held in the server itself.
#'
#'1. No object is identical
#'2. None of values the server variable is present in the encoded values
#'3. All the values are numeric
#'4. The server data and the encoded data are both significantly different (T-test and
#'Mann-Whitney Test, p = 0.99)
#'5. None of the values are within the limit set by the non-disclosure option dsShareServer.near.equal.limit
#'
#'Additionally the encoded data must hold this condition against the server variable:
#'
#'6. The encoded data has a greater number of columns than the server variable

#'@param data.server  - character argument representing the name of a data frame with the original data
#'@param data.encoded - character argument representing the name of a data frame with the encoded data
#'@param data.held.in.server - character argument representing the datasets held in a DataSHIELD server as a data frame
#'@return TRUE if the encoding is suitable. Otherwise false.
#'@export
#'
isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL)
{

  if(is.sharing.allowed())
  {
    suitable.encoding    <- FALSE
    outcome              <- FALSE

    # check validity of parameters
    param.correct        <- idds.are.params.correct(data.server, data.encoded)

    if(param.correct)
    {
      # get data from global environment
      env               <- globalenv()
      settings          <- get.settings()
      encoded           <- get(data.encoded, envir = env)
      data.server       <- create.vector(data.server)

      # check suitable encoding
      suitable.encoding <- sapply(data.server,
                                  function(var.name, encoded, settings, env){return(idds.check.variables(var.name,encoded, settings, env))},
                                  settings = settings,
                                  encoded = encoded,
                                  env = env)

      suitable.encoding <- all(TRUE == suitable.encoding)

      # assign name of encoded data to a possible transfer
      if (suitable.encoding)
      {
        outcome <- param.correct & suitable.encoding
        assignVariable(data.encoded, outcome)
      }
    }
    else
    {
      stop("SERVER::SHARING::ERR:002")
    }

  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }

  return(outcome)
}
