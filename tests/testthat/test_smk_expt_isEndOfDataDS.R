source('options/options_definitions.R')
source("data_files/variables.R")

context("dsShareServer::isEndOfData::expt::isEndOfDataDS")
test_that("no option set",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_a"))
})

set.default.options.to.null()
set.not.allowed()
assignSharingSettingsDS()

test_that("option set, not allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_a"))
})

options(sharing.near.equal.limit = 1000)
options(sharing.allowed = 1)
assignSharingSettingsDS()

test_that("option set, allowed",
{

  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_a"))
  expect_error(isEndOfDataDS(data_encoded = "H"))
})


source("data_files/variables.R")
assignSharingSettingsDS()
data_encoded <- isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data")

rm(list = "transfer", pos = 1)
test_that("option set, allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_A"))
  expect_false(isEndOfDataDS(data_encoded = "df_B"))
  expect_error(isEndOfDataDS(data_encoded = "F")) #exist but was not encoded data as above
  expect_error(isEndOfDataDS(data_encoded = "H")) #does not exist
})

if (exists("transfer", where = 1))
{
  rm(list = "transfer", pos = 1)
}

test_that("option set, allowed",
{
  EOF     <-isEndOfDataDS(data_encoded = "df_B")
  df_B    <- get("df_B", pos = 1)
  expect_false(EOF)
  counter <- 1
  while(!EOF)
  {
    data.transfer <- nextDS("df_B",10)
    counter       <- counter  + 10
    EOF           <- isEndOfDataDS(data_encoded = "df_B")
    expect_equal(counter >= nrow(df_B), EOF)
  }
  expect_true(isEndOfDataDS(data_encoded = "df_B"))
})


#last line
remove.options()
