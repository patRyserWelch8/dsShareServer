
# this function checks the data exchanged to encrypt some parameters
# have been created correctly
epds.is.shared.secrets.valid <- function(settings, shared.secrets)
{
  # sets function variables for checking expected data and set outcome
  correct <- FALSE
  expected.list <- c(settings$encrypted, settings$masking, settings$received,
                     settings$decrypted, settings$index_x, settings$index_y, settings$param_names)

  # check the secrets is a list
  if (is.list(shared.secrets))
  {
    # check all data have been found. If one or more are missing then
    # correct remain to FALSE.
    attributes.exist <- names(shared.secrets) %in% expected.list
    total.correct    <- sum(attributes.exist == TRUE)
    correct          <- (total.correct == length(expected.list))
  }
  return(correct)
}

# compute encoding ratio using the dot product of both information.
epds.compute.encoding.ratio <- function(decrypted.matrix = NULL,param_name, column, row, env = globalenv())
{
  outcome <- 0

  # check suitable classes of arguments
  if (is.matrix(decrypted.matrix) & is.character(param_name) & is.numeric(column) & is.numeric(row))
  {
    # check param_name exists and retrieve it.
    if (exists(param_name, envir = env))
    {
      param <- get(param_name, envir = env)

      # check the number of rows and columns is odd. Even numbers are not suitable
      if ((nrow(decrypted.matrix) %% 2) == 1 &  (ncol(decrypted.matrix) %% 2) == 1)
      {
        # compute the ratio to encode the parameter.
        dot.product    <- decrypted.matrix[row, column]
        outcome        <- param/dot.product
      }
    }
  }
  return(outcome)
}

# encrypt parameters. The column is used to encrypt the parameter
epds.encrypt <- function(concealing.matrix = NULL, column = 0, encoding.ratio=NULL)
{
  outcome <- 0
  if(is.matrix(concealing.matrix) & is.numeric(column)  & is.numeric(encoding.ratio))
  {
    if (column > 0 & column <= ncol(concealing.matrix))
    {
      outcome <- encoding.ratio * concealing.matrix[,column]
    }
  }
  return(outcome)
}

# check the encrypted param are encrypted in the structure
epds.is.encrypted.structure.valid <- function(data.field = "no_data", env = globalenv())
{
  # sets the function variable. obtain again sharing from globalenv to check its validity.
  correct <- FALSE
  sharing <- get.sharing(envir = env)

  if (is.list(sharing))
  {
    list.attributes  <- names(sharing)
    correct <- data.field %in% list.attributes
  }
  return(correct)
}

epds.compute.concealing.matrix <- function(settings, sharing)
{
  return(t(solve(t(sharing[[settings$masking]])) %*% sharing[[settings$encrypted]]))
}

epds.encrypt.param <- function(settings, sharing, index,concealing.matrix, env = globalenv())
{
  param_name <- sharing[[settings$param_names]][index]
  #if(epds.is.param.valid(param_name, env = env))
  #{
    column         <- ceiling(sharing[[settings$index_x]][index] * ncol(sharing[[settings$decrypted]]))
    row            <- ceiling(sharing[[settings$index_y]][index] * nrow(sharing[[settings$decrypted]]))
    encoding.ratio <- epds.compute.encoding.ratio(sharing[[settings$decrypted]], param_name, column, row)
    #column is becomes the index of the row. This is guided by the transpose when the concealing matrix is encrypted
    data           <- epds.encrypt(concealing.matrix,column=row,encoding.ratio)
  #}
}

# encrypt parameters
epds.complete.encryption <- function(settings, sharing, env = globalenv())
{
  # set outcome to false. becomes true, after successful encryption
  outcome <- FALSE

  # checks shared secrets hare been exchanged
  if(epds.is.shared.secrets.valid(settings, shared.secrets = sharing))
  {
    #decrypt encrypted matrix to find concealed values: shared secret
    concealing.matrix <- epds.compute.concealing.matrix(settings, sharing)

    # sets some parameters for encryption
    no.params         <- length(sharing[[settings$param_names]])
    data              <- list()

    # encrypt the parameter
    for (index in 1:no.params)
    {
      data[[index]] <- epds.encrypt.param(settings, sharing, index,sharing[[settings$concealing]], env)
    }

    sharing[[settings$data]] <- data
    assign(settings$name.struct.sharing,sharing, envir = env)
    outcome <- epds.is.encrypted.structure.valid(settings$data, env = env)
  }
  return(outcome)
}

#'@name encryptParamDS
#'@title  encrypt a server parameter
#'@description This server function encrypts a given parameter using a dot product and two shared secrets.
#'@export
encryptParamDS <- function()
{

   if (is.sharing.allowed())
   {
     env      <-  globalenv()
     settings <-  get.settings(envir = env)
     sharing  <-  get.sharing(envir = env)
     if(are.params.created(sharing[[settings$param_names]]))
     {
       return(epds.complete.encryption(settings, sharing, env))
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



