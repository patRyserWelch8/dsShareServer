

ddds.is.received.data.valid <- function(settings, sharing)
{
  correct       <- FALSE
  expected.list <- c(settings$received,settings$masking)
  if (is.list(sharing))
  {
      attributes.exist <- names(sharing) %in% expected.list
      total.correct    <- sum(attributes.exist == TRUE)
      correct          <- (total.correct == length(expected.list))
      if (correct)
      {
         correct       <- correct &
                          is.matrix(sharing[[settings$masking]]) &
                          is.matrix(sharing[[settings$received]])
      }
  }
  return(correct)
}

ddds.decrypt.received.matrix <- function(masking.matrix = NULL, received.matrix = NULL)
{
  result <- NULL

  if(is.matrix(masking.matrix) & is.matrix(received.matrix))
  {
    masking.inverse  <- solve(t(masking.matrix))
    no.col           <- ncol(masking.inverse)
    no.row           <- nrow(received.matrix)
    result           <- matrix(rep(0,no.row * no.col),no.row, no.col)

    if (no.row == no.col)
    {
      result <- masking.inverse %*% received.matrix
    }
  }

  return(result)
}

ddds.is.decrypted.data.valid <- function(settings, sharing)
{
  correct       <- FALSE
  expected.list <- c(settings$received,settings$masking, settings$decrypted)

  if (is.list(sharing))
  {
        attributes.exist <- names(sharing) %in% expected.list
        total.correct    <- sum(attributes.exist == TRUE)
        correct          <- (total.correct == length(expected.list))
   }

  return(correct)
}

#'@name decryptDataDS
#'@title  decrypt data received from another server
#'@description This server function decrypts some received data from a server acting as a "receiver"
#'@export
decryptDataDS   <- function()
{
  outcome       <- FALSE


  if (is.sharing.allowed())
  {
    # sets variables for the decrytion
    env      <- globalenv()
    settings <- get.settings()
    sharing  <- get.sharing()


    if(ddds.is.received.data.valid(settings = settings, sharing = sharing))
    {
      sharing[[settings$decrypted]] <- ddds.decrypt.received.matrix(sharing[[settings$masking]],
                                                                    sharing[[settings$received]])
      assign(get.sharing.name(), sharing, envir = env)
      outcome                       <- ddds.is.decrypted.data.valid(settings, sharing)
    }
    else
    {
      stop("SERVER::ERR::SHARING::007")
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
  return(outcome)
}
