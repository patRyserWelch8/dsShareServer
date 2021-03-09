source('options/options_definitions.R')


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
options(dsSS_sharing.allowed = 1)
assignSharingSettingsDS()

test_that("option set, allowed",
{

  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_a"))
  expect_error(isEndOfDataDS(data_encoded = "H"))
})

source('options/options_definitions.R')
source("data_files/variables.R")


assignSharingSettingsDS()
data_encoded <- isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data")

if(exists("transfer",where = 1))
{
  rm(list = "transfer", pos = 1)
}

options(dsSS_sharing.allowed = 1)
test_that("option set, allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data_encoded = vector_a))
  expect_error(isEndOfDataDS(data_encoded = "vector_A"))
  expect_error(isEndOfDataDS(data_encoded = "df_B"))
  expect_error(isEndOfDataDS(data_encoded = "F")) #exist but was not encoded data as above
  expect_error(isEndOfDataDS(data_encoded = "H")) #does not exist
})

if (exists("transfer", where = 1))
{
  rm(list = "transfer", pos = 1)
}

test_that("option set, allowed",
{
  options(dsSS_sharing.near.equal.limit = 1000)
  options(dsSS_sharing.allowed = 1)

  vector_A <- c(1:71)
  vector_B <- vector_A * 100000
  vector_C <- vector_A + 1

  assign("df_A", data.frame(vector_A,vector_B, list_C), pos = 1)
  assign("df_B", data.frame(vector_A * -10000000 , vector_B * -10000000, vector_C * -10000000), pos = 1)
  assign("df_C", data.frame(vector_A + 1, vector_B + 1, vector_C + 1), pos = 1)
  assign("D", read.csv("data_files/DATASET1.csv", header = TRUE), pos = 1)
  assign("E", read.csv("data_files/DATASET2.csv", header = TRUE), pos = 1)
  assign("F", read.csv("data_files/DATASET3.csv", header = TRUE), pos = 1)
  assign("all.data", rbind(get("D", pos = 1), get("E", pos = 1), get("F", pos = 1)) , pos = 1)
  all.data <- get("all.data", pos= 1)


  # step 1 - set settings
  expect_true(assignSharingSettingsDS())
  expect_true(assignVariableSettingsDS(var_name = "df_B"))


  # step 2 - check encodedDataDS
  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data"))


  # step 3 - iterate through
  EOF <- isEndOfDataDS(data_encoded = "df_B")
  expect_false(EOF)
  while(!EOF)
  {
    data.transfer <- nextDS("df_B",10)
    EOF <- isEndOfDataDS(data_encoded = "df_B")
  }


})


#last line
remove.options()
