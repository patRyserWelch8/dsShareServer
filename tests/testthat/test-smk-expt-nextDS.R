source('options/options_definitions.R')
source("data_files/variables.R")
set.default.options.to.null()

context("dsShareServer::nextDS::smk")
test_that("nothing has been set",
{
  expect_error(nextDS())
  expect_error(nextDS("df_B",10))
})

set.not.allowed()
assignSharingSettingsDS()

test_that("not allowed and nothing has been set",
{
  expect_error(nextDS())
  expect_error(nextDS("df_B",10))
})

set.allowed()
assignSharingSettingsDS()
test_that("allowed and nothing has been set",
{
  expect_error(nextDS())
  expect_error(nextDS("df_B",10))
})


set.allowed()
assignSharingSettingsDS()
source("data_files/variables.R")
test_that("allowed and no encoding set",
{
  expect_error(nextDS())
  expect_error(nextDS("df_B",10))
})


set.default.options.restrictive()
set.allowed()
assignSharingSettingsDS()

source("data_files/variables.R")
assign("all.data", rbind(get("D", pos = 1), get("E", pos = 1), get("F", pos = 1)) , pos = 1)
all.data <- get("all.data", pos= 1)

data.encoded <- isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data")

test_that("allowed and encoding set",
{
  expect_error(nextDS())
  expect_true(is.list(nextDS("df_B",10)))
})

rm(list = "transfer", pos = 1)
set.default.options.restrictive()
set.allowed()
assignSharingSettingsDS()

source("data_files/variables.R")
assign("all.data", rbind(get("D", pos = 1), get("E", pos = 1), get("F", pos = 1)) , pos = 1)
all.data <- get("all.data", pos= 1)

data.encoded <- isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data")

context("dsShareServer::nextDS::expt")
test_that("allowed and no encoding set",
{
  EOF     <-isEndOfDataDS(data_encoded = "df_B")
  df_B    <- get("df_B", pos = 1)
  counter <- 1
  while(!EOF)
  {
    data.transfer <- nextDS("df_B",10)
    counter       <- counter  + 10
    EOF           <- isEndOfDataDS(data_encoded = "df_B")
    transfer      <- get("transfer", pos = 1)
    expect_true(transfer$current_row == counter)
    expect_true(identical(data.transfer$header,"FM1"))
    expect_true(is.numeric(data.transfer$property.a))
    expect_true(is.numeric(data.transfer$property.b))
    expect_true(is.numeric(data.transfer$property.c))
    expect_true(is.numeric(data.transfer$property.d))
    expect_true(data.transfer$property.b == 3)
  }

})
