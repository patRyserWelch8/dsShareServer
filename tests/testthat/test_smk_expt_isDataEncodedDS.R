source('options/options_definitions.R')
source("data_files/variables.R")
set.default.options.to.null()

context("dsShareServer::isDataEncodeDS::expt::are.params.corrrect")
test_that("incorrect arguments",
{
  expect_error(.are.params.correct())
  expect_error(.are.params.correct(data.server = "A"))
  expect_error(.are.params.correct(data.server = "A", data.encoded = "B"))
  expect_error(.are.params.correct(data.server = "D", data.encoded = "E"))
})

set.default.options.not.restrictive()
test_that("incorrect arguments",
{
  expect_error(.are.params.correct())
  expect_error(.are.params.correct(data.server = "A"))
  expect_error(.are.params.correct(data.server = "A", data.encoded = "B"))

})

source("data_files/variables.R")

test_that("correct arguments outcome true",
{
  expect_true(.are.params.correct(data.server = "D", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "vector_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "matrix_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "list_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
})


set.not.allowed()
assignSharingSettingsDS()

test_that("correct arguments outcome true",
{
  expect_true(.are.params.correct(data.server = "D", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "vector_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "matrix_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "list_A", data.encoded = "E", data.held.in.server = "F"))
  expect_true(.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "F"))

  expect_error(.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
})

set.allowed()
assignSharingSettingsDS()
test_that("correct arguments outcome errors",
{
  expect_error(.are.params.correct(data.server = "pi", data.encoded = "E", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "vector_A", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "matrix_A", data.held.in.server = "F"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "list_C", data.held.in.server = "F"))


  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "vector_A"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "matrix_A"))
  expect_error(.are.params.correct(data.server = "df_A", data.encoded = "E", data.held.in.server = "list_C"))
})

source("data_files/variables.R")


context("dsShareServer::isDataEncodeDS::expt::.are.significant.same")
test_that("incorrect arguments",
{
  expect_error(.are.significant.same())
  expect_error(.are.significant.same(server = A))
  expect_error(.are.significant.same(server = A, encoded = B))
})

test_that("expected outcome",
{
  expect_true(.are.significant.same(server = vector_a, encoded = vector_a))
  expect_true(.are.significant.same(server = vector_a, encoded = vector_c))
  expect_false(.are.significant.same(server = vector_b, encoded = vector_c))

  expect_false(.are.significant.same(server = vector_a, encoded = matrix_a))
  expect_false(.are.significant.same(server = vector_a, encoded = matrix_c))
  expect_false(.are.significant.same(server = vector_a, encoded = matrix_b))

  expect_true(.are.significant.same(server = matrix_a, encoded = matrix_a))
  expect_true(.are.significant.same(server = matrix_a, encoded = matrix_c))
  expect_false(.are.significant.same(server = matrix_b, encoded = matrix_c))

  # list have to be unlisted before being passed
  expect_error(.are.significant.same(server = list_a, encoded = list_a))
  expect_error(.are.significant.same(server = list_a, encoded = list_c))
  expect_error(.are.significant.same(server = matrix_a, encoded = list_c))

  # data.framee have to be converted  to  matrices before being passed
  expect_error(.are.significant.same(server = df_a, encoded = df_a))
  expect_error(.are.significant.same(server = df_a, encoded = df_c))
  expect_error(.are.significant.same(server = vector_a, encoded = df_c))

  expect_false(.are.significant.same(server = vector_a, encoded = vector_b))
  expect_false(.are.significant.same(server = vector_a, encoded = matrix_b))

  expect_error (.are.significant.same(server=rep(NA,100),encoded=rep(NA,100) ))
})

context("dsShareServer::isDataEncodeDS::expt::.check.dimension")
test_that("correct arguments",
{
  expect_true(.check.dimension(vector_a, df_A))
  expect_false(.check.dimension(vector_a, as.data.frame(vector_b)))
})

context("dsShareServer::isDataEncodeDS::expt::.are.values.in.limit")
test_that("incorrect arguments",
{
  expect_error(.are.values.in.limit())
  expect_error(.are.values.in.limit(server = A))
  expect_error(.are.values.in.limit(server = A, encoded = B))
  expect_error(.are.values.in.limit(server = A, encoded = B, limit="9999"))
})

test_that("expected outcome",
{

  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 1000))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 1000))
  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 1000))
  expect_false(.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 1000))


  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 0.1))

  #compare by columns. So it is best to comvert to a vector
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_a, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_b, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_c, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_b, encoded = matrix_c, limit = 1000))

  #compare by columns. So it is best to comvert to a vector
  expect_false(.are.values.in.limit(server = df_a, encoded = df_a, limit = 1000))
  expect_false(.are.values.in.limit(server = df_a, encoded = df_b, limit = 1000))
  expect_false(.are.values.in.limit(server = df_a, encoded = df_c, limit = 1000))
  expect_false(.are.values.in.limit(server = df_b, encoded = df_c, limit = 1000))

  #compare by elements of list.  So it is best to comvert to a vector
  expect_false(.are.values.in.limit(server = list_a, encoded = list_a, limit = 1000))
  expect_false(.are.values.in.limit(server = list_a, encoded = list_b, limit = 1000))
  expect_false(.are.values.in.limit(server = list_a, encoded = list_c, limit = 1000))
  expect_false(.are.values.in.limit(server = list_b, encoded = list_c, limit = 1000))
})



context("dsShareServer::isDataEncodeDS::expt::.convert.data")
test_that("incorrect argument ",
{
  expect_error(.convert.data())
})

test_that("incorrect argument ",
{
  expect_true(is.null(.convert.data(NULL)))
  expect_true(is.vector(.convert.data("hi")))
  expect_true(is.vector(.convert.data(3)))
  expect_true(is.vector(.convert.data(pi)))
  expect_true(is.vector(.convert.data(TRUE)))
  expect_true(is.vector(vector_a))
  expect_true(is.vector(.convert.data(vector_A)))
  expect_true(is.list(list_a))
  expect_true(is.vector(.convert.data(list_A)))
  expect_true(is.matrix(matrix_a))
  expect_true(is.vector(.convert.data(matrix_A)))
  expect_true(is.data.frame(df_a))
  expect_true(is.vector(.convert.data(df_a)))
})

context("dsShareServer::isDataEncodeDS::expt::.is.encoded")
test_that("incorrect argument ",
{
 expect_error(.is.encoded())
 expect_error(.is.encoded(A))
 expect_error(.is.encoded(A, B, C))
 expect_error(.is.encoded(vector_a))
})

test_that("expected outcome not restrictive vector",
{
 limit <-  1000
 near_vector_a <- vector_a + 999
 long_vector   <- c(vector_b, near_vector_a)

 expect_equal(.is.encoded(vector_a, vector_a, limit), 1) #identical
 expect_equal(.is.encoded(vector_a, vec_a_char, limit), 2) #value in datasets
 expect_equal(.is.encoded(vector_a, vec_b_char, limit), 3) # character but data are different !
 expect_equal(.is.encoded(vector_a, vector_a_copy, limit), 4) #near equal significantly the same
 expect_equal(.is.encoded(vector_a, near_vector_a, limit), 5) #data are within limit
 expect_equal(.is.encoded(vector_a, vector_b, limit), 6) # same list
})



test_that("expected outcome not restrictive list",
{
  limit <-  1000
  near_list_a <- vector_a + 999
  long_list  <- c(list_b, near_list_a)
  list_a_char <- list(vec_a_char)
  list_b_char <- list(vec_b_char)
  list_a_copy <- list(vector_a_copy)


  expect_equal(.is.encoded(list_a, list_a, limit), 1) #identical
  expect_equal(.is.encoded(list_a, list_a_char, limit), 2) #value in datasets
  expect_equal(.is.encoded(list_a, list_b_char, limit), 3) # character but data are different !
  expect_equal(.is.encoded(list_a, list_a_copy, limit), 4) #near equal significantly the same
  expect_equal(.is.encoded(list_a, near_list_a, limit), 5) #data are within limit
  expect_equal(.is.encoded(list_a, long_list, limit), 6) # same list
})

context("dsShareServer::isDataEncodeDS::expt::..check.encoding.data.frames")
test_that("correct argument ",
{
  limit <- 10000
  expect_true(.check.encoding.data.frames(df_a, df_b, limit))
  expect_false(.check.encoding.data.frames(df_a, df_c, limit))
})

context("dsShareServer::isDataEncodeDS::expt::.check.encoding.variable")
test_that("correct argument ",
{
  limit <- 10000
  expect_false(.check.encoding.variable(vector_a, df_a, limit))
  expect_true(.check.encoding.variable(vector_a, df_b, limit))
  expect_false(.check.encoding.variable(vector_a, df_c, limit))

  expect_false(.check.encoding.variable(vector_b, df_a, limit))
  expect_true(.check.encoding.variable(vector_b, df_b, limit))
  expect_true(.check.encoding.variable(vector_b, df_c, limit))

  expect_false(.check.encoding.variable(vector_c, df_a, limit))
  expect_true(.check.encoding.variable(vector_c, df_b, limit))
  expect_false(.check.encoding.variable(vector_c, df_c, limit))

  limit <- 10000
  expect_false(.check.encoding.variable(list_a, df_a, limit))
  expect_true(.check.encoding.variable(list_a, df_b, limit))
  expect_false(.check.encoding.variable(list_a, df_c, limit))

  expect_false(.check.encoding.variable(list_b, df_a, limit))
  expect_true(.check.encoding.variable(list_b, df_b, limit))
  expect_true(.check.encoding.variable(list_b, df_c, limit))

  expect_false(.check.encoding.variable(list_c, df_a, limit))
  expect_true(.check.encoding.variable(list_c, df_b, limit))
  expect_false(.check.encoding.variable(list_c, df_c, limit))


  limit <- 10000
  expect_false(.check.encoding.variable(matrix_a, df_a, limit))
  expect_true(.check.encoding.variable(matrix_a, df_b, limit))
  expect_false(.check.encoding.variable(matrix_a, df_c, limit))

  expect_true(.check.encoding.variable(matrix_b, df_a, limit))
  expect_true(.check.encoding.variable(matrix_b, df_b, limit))
  expect_true(.check.encoding.variable(matrix_b, df_c, limit))

  expect_false(.check.encoding.variable(matrix_c, df_a, limit))
  expect_true(.check.encoding.variable(matrix_c, df_b, limit))
  expect_false(.check.encoding.variable(matrix_c, df_c, limit))

  expect_false(.check.encoding.variable(df_a, df_a, limit))
  expect_false(.check.encoding.variable(df_b, df_b, limit))
  expect_false(.check.encoding.variable(df_c, df_c, limit))

  expect_false(.check.encoding.variable(df_c, df_a, limit))
  expect_true(.check.encoding.variable(df_a, df_b, limit))
  expect_true(.check.encoding.variable(df_b, df_c, limit))
})


context("dsShareServer::isDataEncodeDS::smk")
test_that("no arguments",
{
    expect_error(isDataEncodedDS())
})

context("dsShareServer::isDataEncodeDS::expt")
test_that("arguments are not correct",
{
  expect_error(isDataEncodedDS(data.server  = "D"))
  expect_error(isDataEncodedDS(data.encoded = "D"))
})


test_that("expected outcome not restrictive",
{
  set.default.options.not.restrictive()
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

test_that("expected outcome not restrictive",
{
  set.default.options.restrictive()
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
  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data"))
})


