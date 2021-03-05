

# validates only the expected elements remains in sharing.
rdds.is.cleaned.structure.valid <- function(expected.list)
{
  correct <- FALSE
  sharing <- get.sharing()
  if (is.list(sharing))
  {
    list.attributes  <- names(sharing)
    attributes.exist <- list.attributes %in% expected.list
    total.incorrect  <- sum(attributes.exist == FALSE)
    total.correct    <- sum(attributes.exist == TRUE)
    correct          <- (total.correct == length(expected.list) & total.incorrect == 0)
  }

  return(correct)
}

#'@name removeEncryptingDataDS
#'@title Remove data used to encrypt some parameters
#'@description This server function can be used to remove the data used to encrypt a parameter between
#'@param master_mode a boolean argument set to true by default
#'a two servers
#'@export
removeEncryptingDataDS <- function(master_mode = TRUE)
{
  outcome <- FALSE
  if(is.sharing.allowed())
  {
    # sets variables used for removal.
    env             <- globalenv()
    settings        <- get.settings(envir = env)
    sharing         <- get.sharing(envir = env)

    # sets the expected list of fields to keep
    expected.list   <- c(settings$no_columns, settings$no_rows, settings$index_x,
                         settings$index_y)
    # sets the list of elements
    if(master_mode)
    {
        expected.list  <- c(settings$data,settings$param_names, expected.list)
    }
    else
    {
      expected.list  <- c(settings$concealing, expected.list)
    }

    # keep the elements listed in expected list
    sharing <- sharing[names(sharing) %in% expected.list]
    assign(settings$name.struct.sharing, sharing, envir = env)

    # check outcome
    outcome <- rdds.is.cleaned.structure.valid(expected.list = expected.list)
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }
  return(outcome)
}
