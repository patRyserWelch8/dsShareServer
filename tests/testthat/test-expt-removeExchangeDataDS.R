
context("removeTransferDataDS::expt::no_settings")
test_that("no_setting",
{
  settings.name <- get.settings.name()
  if(exists(settings.name,where = 1))
  {
    rm(list = settings.name,pos=1)
  }
  expect_equal(exists(settings.name,where = 1),FALSE)
  expect_error(removeTransferDataDS())
})


context("removeTransferDataDS::expt::with_settings")
test_that("with_setting",
{
  settings.name <- get.settings.name()
  assign(settings.name, list(), pos = 1)
  expect_equal(exists(settings.name,where = 1),TRUE)
  expect_equal(removeTransferDataDS(),TRUE)

  settings.name <- get.settings.name()
  assign(settings.name, list(name.struct = "sharing"), pos = 1 )
  expect_equal(removeTransferDataDS(),TRUE)
})



context("removeTransferDataDS::expt::with_sharing")
test_that("with_sharing",
{
  settings.name <- get.settings.name()
  assign(settings.name, list(name.struct = "sharing"), pos = 1 )
  assign("sharing", list(), pos = 1)
  expect_equal(exists(settings.name,where = 1),TRUE)
  expect_equal(removeTransferDataDS(),TRUE)
})

