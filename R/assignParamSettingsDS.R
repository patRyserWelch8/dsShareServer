# generate some ratios to be apply for each coordinate in the matrix. Those ratios
# indicates which rows and columns are use finding the appropriate vectors.
apds.generate.ratios <- function(no.elements = 0, coordinate = 0)
{
  stop    <-  (no.elements == 0) & (coordinate == 0)

  while(!stop)
  {
     # generate a vector of random numbers.
     outcome   <- as.vector(stats::runif(no.elements, min = 0.01, max = 0.99))

     # find the rows or columns index for the coordinates.
     values    <- ceiling(outcome * coordinate)

     # check each number is unique. So no of levels is the same as  the number of elements
     # i.e., parameters.
     no.levels <- length(levels(factor(values)))
     stop <- (no.levels == no.elements)
  }

  return(outcome)
}

# initialise the ratio used for the index of x and y for each parametre
apds.init.coordinates.ratios <- function(settings, sharing, param_names)
{
  if(is.list(sharing))
  {
    # set random number each time it is executed. The random number
    # prevents eavesdroppers to build again the indices.
    sys.time <- as.numeric(Sys.time())
    set.seed(sys.time)
    random.number <- stats::runif (1, min = 1, max = 10^6)

    set.seed(sys.time/random.number)

    # set the ratio in the coordinates. x becomes uses the columns and y the rows. Some transpose
    # in latter steps will set x as column and y as row. Some checks are in place
    # to prevent issues with indices out of bounds.
    sharing[[settings$index_x]]     <- apds.generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_columns]] - 1)
    sharing[[settings$index_y]]     <- apds.generate.ratios(no.elements = length(param_names), coordinate = sharing[[settings$no_rows]] - 1)
    sharing[[settings$param_names]] <- param_names


    return(sharing)
  }
  else
  {
    return(list())
  }
}

# validates the outcome is correct. i.e., indices and param names
# have been created
apds.is.outcome.valid <- function(settings, sharing)
{
  correct <- FALSE

  if (is.list(sharing))
  {
    expected.fields  <- c(settings$index_x, settings$index_y, settings$param_names)
    list.attributes  <- names(sharing)
    attributes.exist <- list.attributes %in% expected.fields
    total.correct    <- sum(attributes.exist == TRUE)
    correct          <- (total.correct == length(expected.fields))
  }

  return(correct)
}

# create a vector for param names.
apds.create.vector <- function (param_names = "")
{
  outcome <- c()
  if(is.character(param_names))
  {
    names.list <- strsplit(param_names,";")
    outcome <- unlist(names.list)
  }
  return(outcome)
}

apds.assign.param.settings <- function(param_names = c())
{
  outcome           <- FALSE
  # init server objects
  env      <- globalenv()
  settings <- get.settings(envir = env)
  sharing  <- get.sharing(envir = env)

  # check encryption has taken place in previous steps
  encrypted.exists  <- settings$encrypted %in% names(sharing)

  if (encrypted.exists)
  {
    #sharing        <- get(settings$name.struct, envir = env) delete
    # initialise coordinates
    sharing         <- apds.init.coordinates.ratios(settings, sharing, param_names)
    outcome         <- apds.is.outcome.valid(settings, sharing)

    if(outcome)
    {
      assign(settings$name.struct.sharing, sharing, envir = env)
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
    param_names.decoded <- apds.create.vector(param_names)

    if(are.params.created(param_names.decoded))
    {
      return(apds.assign.param.settings( param_names.decoded))
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
