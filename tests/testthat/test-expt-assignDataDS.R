source("definition_tests/def_getEncodedDataDS.R")
source('definition_tests/def_sendEncodedDataDS.R')

rm(list = ls(pos = 1),pos = 1)
context("assignDataDS::expt::no_settings")
test_that("no_setting",
{
  expect_error(assignDataDS())
  expect_error(assignDataDS(1))
  expect_error(assignDataDS("FM1",TRUE))
  expect_error(assignDataDS("FM1","123,123","WRONG"))
  expect_error(assignDataDS("FM1","123,123",1,"WRONG" ))
  expect_error(assignDataDS("FM1","123,123",1,13,"WRONG" ))
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,"INCORRECT" ))
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,5 ))

})

options(param.name.struct = "sharing")
options(sharing.allowed = 0)
assignSharingSettingsDS()

context("assignDataDS::expt::not_allowed")
test_that("not_allowed",
{
  expect_error(assignDataDS())
  expect_error(assignDataDS(1))
  expect_error(assignDataDS("FM1",TRUE))
  expect_error(assignDataDS("FM1","123,123","WRONG"))
  expect_error(assignDataDS("FM1","123,123",1,"WRONG" ))
  expect_error(assignDataDS("FM1","123,123",1,13,"WRING" ))
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,"INCORRECT" ))
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,5 ))

})

options(param.name.struct = "sharing")
options(sharing.allowed = 1)
assignSharingSettingsDS()

context("assignDataDS::expt::incorrect parameters")
test_that("parameters",
{
  expect_error(assignDataDS())
  expect_error(assignDataDS(1))
  expect_error(assignDataDS("FM1",TRUE))
  expect_error(assignDataDS("FM1","123,123","WRONG"))
  expect_error(assignDataDS("FM1","123,123",1,"WRONG" ))
  expect_error(assignDataDS("FM1","123,123",1,13,"WRING" ))
  expect_error(assignDataDS("FM1","123,123",1,13,2.3,"INCORRECT"))
})

options(param.name.struct = "sharing")
options(sharing.allowed = 1)

pi_value = 1000
assignSharingSettingsDS()


encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

data <- getDataDS(master_mode = TRUE)
#rm("sharing",pos = 1)

result <- assignDataDS(master_mode = FALSE,data$header,
                                           data$payload,
                                           data$property.a,
                                           data$property.b,
                                           data$property.c,
                                           data$property.d)
receiver.1 <- get("sharing",pos=1)

context("assignDataDS::expt::")
test_that("variables exists",
{
  expect_equal(result,TRUE)
  .test.data.structure(data)
  sharing <- get("sharing", pos=1)
  expect_equal("received" %in% names(sharing),TRUE)
})

#This test does not work with devtools::check!
#context("assignDataDS::expt::.create.matrix")
#test_that("variables exists",
#{
#  .test.create.matrix.parameters.correct(as.character(data$payload), data$property.b)
#  .test.create.matrix.parameters.incorrect() #NAs warning. testing incorrect situation
#})

#context("assignDataDS::expt::.save.matrix")
#test_that("variables exists",
#{
#  .test.save.matrix.parameters.correct()
#  .test.save.matrix.parameters.incorrect()
#})




