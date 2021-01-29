
.has.correct.data <- function()
{
  encrypted.exists <- FALSE

  if (exists(settings$name.struct,where = 1))
  {
      sharing           <- get(settings$name.struct, pos = 1)
      encrypted.exists  <- settings$encrypted %in% names(sharing)
  }

  return(encrypted.exists)
}

.generate.ratios <- function(no.elements = 0, coordinate = 0)
{
  stop    <-  no.elements == 0 & coordinate == 0

  while(!stop)
  {
     outcome   <- as.vector(stats::runif(no.elements, min = 0.01, max = 0.99))
     values    <- ceiling(outcome * coordinate)
     no.levels <- length(levels(factor(values)))
     stop <- (no.levels == no.elements)
  }

  return(outcome)
}

.init.coordinates.ratios <- function(param_names, sharing)
{
  if(is.list(sharing))
  {
    sys.time <- as.numeric(Sys.time())
    set.seed(sys.time)
    random.number <- stats::runif (1, min = 1, max = 10^6)

    set.seed(sys.time/random.number)
    sharing[[settings$index_x]]     <- .generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_columns]] - 1)
    sharing[[settings$index_y]]     <- .generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_rows]] - 1)
    sharing[[settings$param_names]] <- param_names


    return(sharing)
  }
  else
  {
    return(list())
  }
}

.is.outcome.valid <- function(sharing, expected.fields)
{
  correct <- FALSE

  if (is.list(sharing) & is.vector(expected.fields))
  {
    list.attributes <- names(sharing)
    attributes.exist <- list.attributes %in% expected.fields
    total.correct = sum(attributes.exist == TRUE)
    correct <- total.correct == length(expected.fields)
  }

  return(correct)
}

.create.vector <- function (param_names = "")
{
  outcome <- c()
  if(is.character(param_names))
  {
    names.list <- strsplit(param_names,";")
    outcome <- unlist(names.list)
  }
  return(outcome)
}

.assignParamSettings <- function(param_names = "", settings)
{
  outcome <-FALSE
  params <- .create.vector(param_names)
  if (.has.correct.data())
  {
    sharing         <- get(settings$name.struct, pos = 1)
    sharing         <-.init.coordinates.ratios(params, sharing)
    expected.fields <- c(settings$index_x, settings$index_y, settings$param_names)
    outcome         <- .is.outcome.valid(sharing,expected.fields)

    if(outcome)
    {
      assign(settings$name.struct, sharing, pos = 1)
    }
  }
  return(outcome)
}



#'@name   assignParamSettingsDS
#'@title  assigns some settings used to encrypt and decrypt the parameters
#'@description This server function sets some settings specific to the parameters encryption and decryption mechanisms.
#'The latter should identify a column and row for each parameter in some matrices. The row and column is disclosive. So, it remains
#'on the server and cannot be analysed directly.
#'@param param_names  character vector. Name of the server parameter to encrypt.
#'@export
assignParamSettingsDS <- function(param_names = "")
{
  if (is.sharing.allowed())
  {
    param_names.decoded <- unlist(strsplit(as.character(param_names),";"))
    settings            <- get("settings", pos = 1)

    if(are.params.created(param_names.decoded))
    {
      outcome <- .assignParamSettings(param_names, settings)

      return(outcome)
    }
    else
    {
      stop("SERVER::ERR::SHARING::008")
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }

}
