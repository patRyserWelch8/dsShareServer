context("concatDataVariable::smk::incorrect_param")
test_that("concatDataVariable::smk::incorrect_param",
{
  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  expect_error(concatDataToVariableDS())
  expect_error(concatDataToVariableDS("1"))
  expect_error(concatDataToVariableDS(data.frame()))
  expect_error(concatDataToVariableDS(NULL))
  expect_error(concatDataToVariableDS("written.var"))
  expect_error(concatDataToVariableDS("written.var", 1))
  expect_error(concatDataToVariableDS("written.var", NULL))
  expect_error(concatDataToVariableDS("written.var", "matrix"))
  expect_error(concatDataToVariableDS("written.var", "matrix", "TRUE TO MYSELF"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, 1))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header", 1))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header","payload"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header","payload", "0"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header","payload", 1, "0"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header","payload", 1, 1, "0"))
  expect_error(concatDataToVariableDS("written.var", "matrix", TRUE, "header","payload", 1, 1, 1, "0"))
  expect_error(concatDataToVariableDS("", "", TRUE, "","", 0, 0, 0, 0))

  rm(list = ls(pos = 1), pos = 1)
  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 0)
  expect_error(concatDataToVariableDS("", "", TRUE, "","", 0, 0, 0, 0))

  })

context("concatDataVariable::expt::correct_param")
test_that("concatDataVariable::expt::correct_param",
{
  # one row
  rm(list = ls(pos = 1), pos = 1)
  data          <- as.character(paste(stats::runif(11 * 1, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())


  expect_true(concatDataToVariableDS("written.var",
                                     "matrix",
                                     FALSE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))

  print(get("written.var", pos = 1))

  data          <- as.character(paste(stats::runif(11 * 1, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)


  expect_true(concatDataToVariableDS("written.var",
                                     "matrix",
                                     FALSE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))
  print(get("written.var", pos = 1))


  # variable does not exists on the server
  rm(list = ls(pos = 1), pos = 1)
  data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  expect_true(concatDataToVariableDS("written.var",
                                     "matrix",
                                     FALSE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))


  expect_true(exists("written.var", where = 1))
  expect_true(is.matrix(get("written.var", pos = 1)))

  # variable does not exists on the server
  rm(list = ls(pos = 1), pos = 1)
  data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  expect_true(concatDataToVariableDS("written.var_1",
                                     "matrix",
                                     TRUE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))

  expect_true(exists("written.var_1", where = 1))
  expect_true(is.matrix(get("written.var_1", pos = 1)))

  # variable does not exists on the server
  rm(list = ls(pos = 1), pos = 1)
  data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  expect_true(concatDataToVariableDS("written.var_2",
                                     "data.frame",
                                     TRUE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))

  expect_true(exists("written.var_2", where = 1))
  expect_true(is.data.frame(get("written.var_2", pos = 1)))

  # variable does not exists on the server
  rm(list = ls(pos = 1), pos = 1)
  data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
  size          <- as.numeric(utils::object.size(data))
  no.columns    <- 11
  no.rows       <- 13
  index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
  timestamp     <- as.numeric(Sys.time()) / size
  return.value  <- list(header = "FM2" ,
                        payload = data,
                        property.a = size,
                        property.b = no.columns,
                        property.c = timestamp,
                        property.d = index/timestamp)

  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  expect_true(concatDataToVariableDS("written.var_3",
                                     "data.frame",
                                     FALSE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))

  expect_true(exists("written.var_3", where = 1))
  expect_true(is.data.frame(get("written.var_3", pos = 1)))


  expect_error(concatDataToVariableDS("written.var_4",
                                     "list",
                                     FALSE,
                                     return.value$header,
                                     return.value$payload,
                                     return.value$property.a,
                                     return.value$property.b,
                                     return.value$property.c,
                                     return.value$property.d))

  expect_false(exists("written.var_4", where = 1))
  expect_error(is.list(get("written.var_4", pos = 1)))

  # repeat several times the process
  rm(list = ls(pos = 1), pos = 1)
  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  for(i in 1:3)
  {
    data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
    size          <- as.numeric(utils::object.size(data))
    no.columns    <- 11
    no.rows       <- 13
    index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
    timestamp     <- as.numeric(Sys.time()) / size
    return.value  <- list(header = "FM2" ,
                          payload = data,
                          property.a = size,
                          property.b = no.columns,
                          property.c = timestamp,
                          property.d = index/timestamp)



    expect_true(concatDataToVariableDS("written.var_5",
                                       "data.frame",
                                       TRUE,
                                       return.value$header,
                                       return.value$payload,
                                       return.value$property.a,
                                       return.value$property.b,
                                       return.value$property.c,
                                       return.value$property.d))

    expect_true(exists("written.var_5", where = 1))
    data <- get("written.var_5", pos = 1)
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), no.rows)

  }

  # repeat several times the process
  rm(list = ls(pos = 1), pos = 1)
  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  expect_true(assignSharingSettingsDS())

  for(j in 1:3)
  {
    data          <- as.character(paste(stats::runif(11 *13, 100000, 400000),sep="", collapse=";"))
    size          <- as.numeric(utils::object.size(data))
    no.columns    <- 11
    no.rows       <- 13
    index         <- ceiling(stats::runif(1, min = 0, max = no.columns))
    timestamp     <- as.numeric(Sys.time()) / size
    return.value  <- list(header = "FM2" ,
                          payload = data,
                          property.a = size,
                          property.b = no.columns,
                          property.c = timestamp,
                          property.d = index/timestamp)



    expect_true(concatDataToVariableDS("written.var_6",
                                       "data.frame",
                                       FALSE,
                                       return.value$header,
                                       return.value$payload,
                                       return.value$property.a,
                                       return.value$property.b,
                                       return.value$property.c,
                                       return.value$property.d))

    expect_true(exists("written.var_6", where = 1))
    data <- get("written.var_6", pos = 1)
    expect_true(is.data.frame(data))

    expect_equal(nrow(data), j * no.rows)

  }





})
