# compute and encode coordinates (obfuscation)
gcds.compute.coordinates <- function(env = globalenv())
{
  # init values for obfuscation. By default FM2 encoding occurs
  return.value <- encode.data.no.sharing()
  settings     <- get.settings(envir = env)
  sharing      <- get.sharing()

  # check sharing has the appropriate coordinates computed in previous steps
  value.exists <- all(c(settings$index_x, settings$index_y) %in% names(sharing))

  # obfuscate if exists
  if(value.exists)
  {
    # set data required for obfuscation
    data           <- c(sharing[[settings$index_x]],sharing[[settings$index_y]])
    no.params      <- length(sharing[[settings$index_x]])

    # generate random data

    repeat
    {
      seed <- generate.secure.seed(settings)
      if(seed < .Machine$integer.max)
      {
        break()
      }
    }
    set.seed(seed)
    random.data    <- stats::runif((settings$min_rows * settings$min_columns) + 1 - (2 * no.params), min = 0, max = 2)

    # obfuscate
    encrypted.data <- c(random.data[1:(length(random.data)/2)],
                            data,
                            random.data[((length(random.data)/2)+1):length(random.data)])

    # encode in suitable format for transfer
    repeat
    {
      seed <- generate.secure.seed(settings)
      if(seed < .Machine$integer.max)
      {
        break()
      }
    }
    set.seed(seed)
    index        <- stats::runif(1, min = 1, max= 100)
    return.value <- encode.data.with.sharing(encrypted.data, length(sharing[[settings$index_x]]), index)
  }
  return(return.value)
}

#'@name getCoordinatesDS
#'@title  Retrieves some data related to some coordinates used to encrypt the parameter
#'@description This server function retrieves some encrypted coordinates to be passed onto the analysis computer
#'@return a list made of a header, a payload, and four properties[a-d]
#'@details Some encoded data are transformed into a suitable format to be transferred from a DataSHIELD server to a DataSHIELD client. The property.a indicates
#'the size of the data at encoding time. Property b is the number of coordinates and property c a timestamp. property d a random number.
#'@seealso \link[dsShareServer]{assignDataDS} \link[dsShareServer]{getCoordinatesDS}
#'@export
getCoordinatesDS <- function()
{
  # check the sharing is allowed
  if (is.sharing.allowed())
  {
    # get the global env and obfuscate coordinates
    env          <- globalenv()
    encoded.data <- gcds.compute.coordinates(env = env)

    # check the obfuscation has occurred
    if(identical(encoded.data$header, "FM2"))
    {
      stop("SERVER::ERR::PARAM::004")
    }
    else
    {
      return(encoded.data)
    }
  }
  else
  {
    stop("SERVER::ERR::SHARING::001")
  }



}
