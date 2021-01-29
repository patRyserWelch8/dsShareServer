# Boolean function that checks whether the arguments of the functions are suitable
# for the comparison. Check the type and server variables have been created as data frame. Tibbles wrappes data frames.
.are.params.correct <- function(data.server = NULL, data.encoded = NULL, data.held.in.server = NULL)
{
  outcome <- FALSE
  if(is.null(.Options$sharing.near.equal.limit) || is.null(.Options$sharing.allowed))
  {
    stop("SERVER::ERR:SHARE::003")
  }
  else if(is.character(data.server) & is.character(data.encoded) & is.character(data.held.in.server))
  {
    if(exists(data.server, where = 1) &
       exists(data.encoded, where = 1) &
       exists(data.held.in.server, where = 1))
    {
       server         <- get(data.server, pos = 1)
       encoded        <- get(data.encoded, pos = 1)
       held.in.server <- get(data.held.in.server, pos = 1)

       if (!is.data.frame(encoded))
       {
         stop("SERVER::ERR:SHARE::005")
       }

       if (!is.data.frame(held.in.server))
       {
         stop("SERVER::ERR:SHARE::006")
       }

       correct.format <- is.data.frame(server) || is.list(server) || is.matrix(server) || (length(server) > 1)
       if(!correct.format)
       {
         stop("SERVER::ERR:SHARE::007")
       }


       outcome        <- correct.format &
                         is.data.frame(encoded) &
                         is.data.frame(held.in.server)
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
.are.significant.same <- function(server, encoded)
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
      t    <- stats::t.test(server, encoded, conf.level = 0.99, na.action=stats::na.omit)
      mann <- stats::wilcox.test(server, encoded, conf.level = 0.99)
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
.are.values.in.limit <- function(server, encoded, limit)
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
.is.encoded <- function(server, encoded, limit)
{
  #init function variables
  step      <- 0
  max       <- 5
  is.failed <- FALSE
  continue  <- TRUE

  # convert into vectors data passed
  server.data     <- .convert.data(server)
  encoded.data    <- .convert.data(encoded)

  # check encoding
  while (continue)
  {
    is.failed <- switch(step + 1,
                        identical(server.data, encoded.data), # 1 identical variables
                        any(server.data %in% encoded.data), #2 some values are present in both datasets
                        !is.numeric(encoded.data), #3 has some non-numeric values
                        .are.significant.same(server.data, encoded.data), # 4 data are significantly the same at the point of centrality
                        .are.values.in.limit(server.data, encoded.data, limit)) #5 data are with the limit min, max, mean, median, IQR
    step     <- step + 1
    continue <- !is.failed & step < max
  }

  #!is.failed add 1, when  is.failed is false. Otherwise 0, when it is TRUE
  return(step + !is.failed)
}

# This function checks the server variable is encoded suitably.
.check.encoding.variable <- function(server,encoded, limit)
{
  outcome <- FALSE

  if (is.data.frame(server))
  {
    outcome <- .check.encoding.data.frames(server, encoded, limit)
  }
  else if (is.list(server) || is.matrix(server) || is.vector(server))
  {
      no_steps <- .is.encoded(server,encoded,limit)
      outcome  <-  (no_steps == 6)
  }
  return(outcome)
}

# This function checks the a data frame is suitable encoded every column of a server is
# checked after each column of the server dataframe.
.check.encoding.data.frames <- function(server, encoded, limit)
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
            no_steps <- .is.encoded(server[j],encoded[i],limit)
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
.convert.data <- function(data)
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
.check.dimension <- function(server, encoded)
{
  outcome <- FALSE
  if (is.list(server))
  {
    lengths <- unlist(lapply(list_A,length))
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
.set.settings <- function(outcome = FALSE, data.encoded)
{
  if(exists("settings", where = 1))
  {
    settings                   <- get("settings", pos = 1)
    settings$encoded.data      <- outcome
    settings$encoded.data.name <- data.encoded
    assign("settings", settings, pos = 1)
  }
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
isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL, data.held.in.server = NULL)
{


  if(is.sharing.allowed())
  {
    is.encoded.data      <- FALSE
    is.encoded.variable  <- FALSE
    outcome              <- FALSE
    param.correct        <- .are.params.correct(data.server, data.encoded, data.held.in.server )

    if(param.correct)
    {
        # get data from global environment
      server         <- get(data.server,  pos = 1)
      encoded        <- get(data.encoded, pos = 1)
      held.in.server <- get(data.held.in.server, pos = 1)
      limit          <- getOption("sharing.near.equal.limit")

      if(.check.dimension(server, encoded))
      {
        is.encoded.variable <- .check.encoding.variable(server, encoded, limit)
        if(is.encoded.variable)
        {
          is.encoded.data <- .check.encoding.data.frames(held.in.server,encoded,limit)
        }
      }
      outcome <- is.encoded.data & is.encoded.variable
      .set.settings(outcome, data.encoded)
    }
    else
    {
      stop("SERVER::ERR::SHARING::002")
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }

  return(outcome)
}
