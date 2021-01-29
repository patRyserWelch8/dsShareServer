context("dsShareServer::smk::setup")
test_that('correct assignment',
{
  expect_true(exists("D", where = 1))
  expect_true(exists("E", where = 1))
  expect_true(exists("F", where = 1))
})
