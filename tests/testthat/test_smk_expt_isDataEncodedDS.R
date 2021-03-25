source('options/options_definitions.R')
source("data_files/variables.R")

rm(list = ls(pos = 1), pos = 1)

context("dsShareServer::isDataEncodeDS::expt::are.params.corrrect")
test_that("incorrect arguments",
{
  expect_error(idds.are.params.correct())
  expect_error(idds.are.params.correct(data.server = "A"))
  expect_error(idds.are.params.correct(data.server = "A", data.encoded = "B"))
  expect_error(idds.are.params.correct(data.server = "D", data.encoded = "E"))
})

options(dsSS_sharing.near.equal.limit = 0.01)
options(dsSS_param.name.struct = "sharing_testing")
options(dsSS_sharing.allowed = 1)
options(dsSS_settings = "settings_ds_share")

test_that("incorrect arguments",
{

  expect_error(idds.are.params.correct())
  expect_error(idds.are.params.correct(data.server = "A"))
  expect_error(idds.are.params.correct(data.server = "A", data.encoded = "B"))

})

source("data_files/variables.R")

test_that("correct arguments outcome true",
{
  expect_true(idds.are.params.correct(data.server = "D", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "vector_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "matrix_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "list_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
})


options(dsSS_sharing.near.equal.limit = 0.01)
options(dsSS_param.name.struct = "sharing_testing")
options(dsSS_sharing.allowed = 0)
options(dsSS_settings = "settings_ds_share")

assignSharingSettingsDS()

test_that("correct arguments outcome true",
{
  expect_true(idds.are.params.correct(data.server = "D", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "vector_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "matrix_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "list_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(idds.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "F"))

  expect_error(idds.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
})

options(dsSS_sharing.near.equal.limit = 0.01)
options(dsSS_param.name.struct = "sharing_testing")
options(dsSS_sharing.allowed = 1)
options(dsSS_settings = "settings_ds_share")
assignSharingSettingsDS()
test_that("correct arguments outcome errors",
{
  expect_error(idds.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "list_C", data.held.in.server = "F"))


  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "vector_A"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "matrix_A"))
  expect_error(idds.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "list_C"))
})

source("data_files/variables.R")


context("dsShareServer::isDataEncodeDS::expt::.are.significant.same")
test_that("incorrect arguments",
{
  expect_error(idds.are.significant.same())
  expect_error(idds.are.significant.same(server = A))
  expect_error(idds.are.significant.same(server = A, encoded = B))
})

test_that("expected outcome",
{
  expect_true(idds.are.significant.same(server = vector_a, encoded = vector_a))
  expect_true(idds.are.significant.same(server = vector_a, encoded = vector_c))
  expect_false(idds.are.significant.same(server = vector_b, encoded = vector_c))

  expect_false(idds.are.significant.same(server = vector_a, encoded = matrix_a))
  expect_false(idds.are.significant.same(server = vector_a, encoded = matrix_c))
  expect_false(idds.are.significant.same(server = vector_a, encoded = matrix_b))

  expect_true(idds.are.significant.same(server = matrix_a, encoded = matrix_a))
  expect_true(idds.are.significant.same(server = matrix_a, encoded = matrix_c))
  expect_false(idds.are.significant.same(server = matrix_b, encoded = matrix_c))

  # list have to be unlisted before being passed
  expect_error(idds.are.significant.same(server = list_a, encoded = list_a))
  expect_error(idds.are.significant.same(server = list_a, encoded = list_c))
  expect_error(idds.are.significant.same(server = matrix_a, encoded = list_c))

  # data.framee have to be converted  to  matrices before being passed
  expect_error(idds.are.significant.same(server = df_a, encoded = df_a))
  expect_error(idds.are.significant.same(server = df_a, encoded = df_c))
  expect_error(idds.are.significant.same(server = vector_a, encoded = df_c))

  expect_false(idds.are.significant.same(server = vector_a, encoded = vector_b))
  expect_false(idds.are.significant.same(server = vector_a, encoded = matrix_b))

  expect_error (idds.are.significant.same(server=rep(NA,100),encoded=rep(NA,100) ))
})

context("dsShareServer::isDataEncodeDS::expt::.check.dimension")
test_that("correct arguments",
{
  expect_true(idds.check.dimension(vector_a, df_A))
  expect_false(idds.check.dimension(vector_a, as.data.frame(vector_b)))
})

context("dsShareServer::isDataEncodeDS::expt::.are.values.in.limit")
test_that("incorrect arguments",
{
  expect_error(idds.are.values.in.limit())
  expect_error(idds.are.values.in.limit(server = A))
  expect_error(idds.are.values.in.limit(server = A, encoded = B))
  expect_error(idds.are.values.in.limit(server = A, encoded = B, limit="9999"))
})

test_that("expected outcome",
{

  expect_true(idds.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 1000))
  expect_false(idds.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 1000))
  expect_true(idds.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 1000))
  expect_false(idds.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 1000))


  expect_true(idds.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 0.1))
  expect_false(idds.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 0.1))
  expect_false(idds.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 0.1))
  expect_false(idds.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 0.1))

  #compare by columns. So it is best to comvert to a vector
  expect_error(idds.are.values.in.limit(server = matrix_a, encoded = matrix_a, limit = 1000))
  expect_error(idds.are.values.in.limit(server = matrix_a, encoded = matrix_b, limit = 1000))
  expect_error(idds.are.values.in.limit(server = matrix_a, encoded = matrix_c, limit = 1000))
  expect_error(idds.are.values.in.limit(server = matrix_b, encoded = matrix_c, limit = 1000))

  #compare by columns. So it is best to comvert to a vector
  expect_false(idds.are.values.in.limit(server = df_a, encoded = df_a, limit = 1000))
  expect_false(idds.are.values.in.limit(server = df_a, encoded = df_b, limit = 1000))
  expect_false(idds.are.values.in.limit(server = df_a, encoded = df_c, limit = 1000))
  expect_false(idds.are.values.in.limit(server = df_b, encoded = df_c, limit = 1000))

  #compare by elements of list.  So it is best to comvert to a vector
  expect_false(idds.are.values.in.limit(server = list_a, encoded = list_a, limit = 1000))
  expect_false(idds.are.values.in.limit(server = list_a, encoded = list_b, limit = 1000))
  expect_false(idds.are.values.in.limit(server = list_a, encoded = list_c, limit = 1000))
  expect_false(idds.are.values.in.limit(server = list_b, encoded = list_c, limit = 1000))
})



context("dsShareServer::isDataEncodeDS::expt::.convert.data")
test_that("incorrect argument ",
{
  expect_error(idds.convert.data())
})

test_that("incorrect argument ",
{
  expect_true(is.null(idds.convert.data(NULL)))
  expect_true(is.vector(idds.convert.data("hi")))
  expect_true(is.vector(idds.convert.data(3)))
  expect_true(is.vector(idds.convert.data(pi)))
  expect_true(is.vector(idds.convert.data(TRUE)))
  expect_true(is.vector(vector_a))
  expect_true(is.vector(idds.convert.data(vector_A)))
  expect_true(is.list(list_a))
  expect_true(is.vector(idds.convert.data(list_A)))
  expect_true(is.matrix(matrix_a))
  expect_true(is.vector(idds.convert.data(matrix_A)))
  expect_true(is.data.frame(df_a))
  expect_true(is.vector(idds.convert.data(df_a)))
})

context("dsShareServer::isDataEncodeDS::expt::.is.encoded")
test_that("incorrect argument ",
{
 expect_error(idds.is.encoded())
 expect_error(idds.is.encoded(A))
 expect_error(idds.is.encoded(A, B, C))
 expect_error(idds.is.encoded(vector_a))
})

test_that("expected outcome not restrictive vector",
{
 limit <-  1000
 near_vector_a <- vector_a + 999
 long_vector   <- c(vector_b, near_vector_a)

 expect_equal(idds.is.encoded(vector_a, vector_a, limit), 1) #identical
 expect_equal(idds.is.encoded(vector_a, vec_a_char, limit), 2) #value in datasets
 expect_equal(idds.is.encoded(vector_a, vec_b_char, limit), 3) # character but data are different !
 expect_equal(idds.is.encoded(vector_a, vector_a_copy, limit), 4) #near equal significantly the same
 expect_equal(idds.is.encoded(vector_a, near_vector_a, limit), 5) #data are within limit
 expect_equal(idds.is.encoded(vector_a, vector_b, limit), 6) # same list
})



test_that("expected outcome not restrictive list",
{
  limit <-  1000
  near_list_a <- vector_a + 999
  long_list  <- c(list_b, near_list_a)
  list_a_char <- list(vec_a_char)
  list_b_char <- list(vec_b_char)
  list_a_copy <- list(vector_a_copy)


  expect_equal(idds.is.encoded(list_a, list_a, limit), 1) #identical
  expect_equal(idds.is.encoded(list_a, list_a_char, limit), 2) #value in datasets
  expect_equal(idds.is.encoded(list_a, list_b_char, limit), 3) # character but data are different !
  expect_equal(idds.is.encoded(list_a, list_a_copy, limit), 4) #near equal significantly the same
  expect_equal(idds.is.encoded(list_a, near_list_a, limit), 5) #data are within limit
  expect_equal(idds.is.encoded(list_a, long_list, limit), 6) # same list
})

context("dsShareServer::isDataEncodeDS::expt::..check.encoding.data.frames")
test_that("correct argument ",
{
  limit <- 10000
  expect_true(idds.check.encoding.data.frames(df_a, df_b, limit))
  expect_false(idds.check.encoding.data.frames(df_a, df_c, limit))
})

context("dsShareServer::isDataEncodeDS::expt::.check.encoding.variable")
test_that("correct argument ",
{
  limit <- 10000
  expect_false(idds.check.encoding.variable(vector_a, df_a, limit))
  expect_true(idds.check.encoding.variable(vector_a, df_b, limit))
  expect_false(idds.check.encoding.variable(vector_a, df_c, limit))

  expect_false(idds.check.encoding.variable(vector_b, df_a, limit))
  expect_true(idds.check.encoding.variable(vector_b, df_b, limit))
  expect_true(idds.check.encoding.variable(vector_b, df_c, limit))

  expect_false(idds.check.encoding.variable(vector_c, df_a, limit))
  expect_true(idds.check.encoding.variable(vector_c, df_b, limit))
  expect_false(idds.check.encoding.variable(vector_c, df_c, limit))

  limit <- 10000
  expect_false(idds.check.encoding.variable(list_a, df_a, limit))
  expect_true(idds.check.encoding.variable(list_a, df_b, limit))
  expect_false(idds.check.encoding.variable(list_a, df_c, limit))

  expect_false(idds.check.encoding.variable(list_b, df_a, limit))
  expect_true(idds.check.encoding.variable(list_b, df_b, limit))
  expect_true(idds.check.encoding.variable(list_b, df_c, limit))

  expect_false(idds.check.encoding.variable(list_c, df_a, limit))
  expect_true(idds.check.encoding.variable(list_c, df_b, limit))
  expect_false(idds.check.encoding.variable(list_c, df_c, limit))


  limit <- 10000
  expect_false(idds.check.encoding.variable(matrix_a, df_a, limit))
  expect_true(idds.check.encoding.variable(matrix_a, df_b, limit))
  expect_false(idds.check.encoding.variable(matrix_a, df_c, limit))

  expect_true(idds.check.encoding.variable(matrix_b, df_a, limit))
  expect_true(idds.check.encoding.variable(matrix_b, df_b, limit))
  expect_true(idds.check.encoding.variable(matrix_b, df_c, limit))

  expect_false(idds.check.encoding.variable(matrix_c, df_a, limit))
  expect_true(idds.check.encoding.variable(matrix_c, df_b, limit))
  expect_false(idds.check.encoding.variable(matrix_c, df_c, limit))

  expect_false(idds.check.encoding.variable(df_a, df_a, limit))
  expect_false(idds.check.encoding.variable(df_b, df_b, limit))
  expect_false(idds.check.encoding.variable(df_c, df_c, limit))

  expect_false(idds.check.encoding.variable(df_c, df_a, limit))
  expect_true(idds.check.encoding.variable(df_a, df_b, limit))
  expect_true(idds.check.encoding.variable(df_b, df_c, limit))
})


context("dsShareServer::isDataEncodeDS::smk")
test_that("no arguments",
{
    expect_error(isDataEncodedDS())
})


test_that("use mtcars and encoded data",
{

   options(dsSS_sharing.near.equal.limit = 1000)
   options(dsSS_param.name.struct = "sharing_testing")
   options(dsSS_sharing.allowed = 1)
   options(dsSS_settings = "settings_ds_share")

   expect_true(assignDemoDataDS())

   expect_true(exists("datashield.mtcars.data", where = 1))
   expect_true(exists("datashield.encrypted.data", where = 1))
   assign("dataset1", read_csv("data_files/DATASET1.csv"), pos = 1)
   assign("dataset2", read_csv("data_files/DATASET2.csv"), pos = 1)
   assign("dataset3", read_csv("data_files/DATASET3.csv"), pos = 1)

   expect_true(isDataEncodedDS(data.server = "datashield.mtcars.data", data.encoded = "datashield.encrypted.data", data.held.in.server = "dataset1"))



   expect_true(isDataEncodedDS(data.server = "datashield.mtcars.data", data.encoded = "datashield.encrypted.data", data.held.in.server = "dataset2"))



   expect_true(isDataEncodedDS(data.server = "datashield.mtcars.data", data.encoded = "datashield.encrypted.data", data.held.in.server = "dataset3"))


})



context("dsShareServer::isDataEncodeDS::expt")
test_that("arguments are not correct",
{
  expect_error(isDataEncodedDS(data.server  = "D"))
  expect_error(isDataEncodedDS(data.encoded = "D"))
})

options(dsSS_sharing.near.equal.limit = 0.01)
options(dsSS_param.name.struct = "sharing_testing")
options(dsSS_sharing.allowed = 1)
options(dsSS_settings = "settings_ds_share")
assignSharingSettingsDS()
test_that("expected outcome not restrictive",
{

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "vector_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_B", data.encoded = "list_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_C", data.encoded = "list_C", data.held.in.server = "F"))

  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_B", data.encoded = "df_B", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_C", data.encoded = "df_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_C", data.encoded = "matrix_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "list_B", data.held.in.server = "F"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "df_B", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "matrix_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "list_C", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "df_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_B", data.encoded = "list_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_C", data.held.in.server = "F"))

  expect_true(isDataEncodedDS(data.server = "df_A", data.encoded = "df_B", data.held.in.server = "F"))
  expect_true(isDataEncodedDS(data.server = "df_B", data.encoded = "df_C", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_C", data.held.in.server = "F"))

  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_C", data.held.in.server = "F"))
})

options(dsSS_sharing.near.equal.limit = 1000000)
options(dsSS_param.name.struct = "sharing_testing")
options(dsSS_sharing.allowed = 1)
options(dsSS_settings = "settings_ds_share")
assignSharingSettingsDS()
test_that("expected outcome restrictive",
{

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "vector_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_B", data.encoded = "list_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_C", data.encoded = "list_C", data.held.in.server = "F"))

  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_B", data.encoded = "df_B", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_C", data.encoded = "df_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_C", data.encoded = "matrix_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "list_B", data.held.in.server = "F"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "df_B", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "matrix_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_C", data.encoded = "list_C", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "df_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_C", data.held.in.server = "F"))

  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_B", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_B", data.encoded = "list_C", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server = "list_A", data.encoded = "list_C", data.held.in.server = "F"))

  expect_true(isDataEncodedDS(data.server = "df_A", data.encoded = "df_B", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_B", data.encoded = "df_C", data.held.in.server = "F"))
  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_C", data.held.in.server = "F"))

  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_C", data.held.in.server = "F"))
  expect_true(isDataEncodedDS(data.server = "vector_small", data.encoded = "df_B", data.held.in.server = "all.data"))
})


