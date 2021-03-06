rm(list=ls(pos = 1),pos=1)

context("assignParamSettingsDS::smk::incorrect_outcome")
test_that("everything is incorrect",
{

  expect_error(assignParamSettingsDS())
  expect_error(assignParamSettingsDS(123))
  expect_equal(are.params.created(param_names = c("first_var")), FALSE)
  expect_equal(are.params.created(param_names = c("first_var", "second_var")),FALSE)
  expect_equal(are.params.created(param_names = c("first_var", "second_var","third_var")),FALSE)
  expect_equal(are.params.created(param_names = c("first_var",1)), FALSE)
  expect_equal(are.params.created(param_names = c(1,2,4)),FALSE)
  expect_equal(are.params.created(param_names = "hello"),FALSE)
  expect_equal(are.params.created(param_names = TRUE),FALSE)
})

context("assignParamSettingsDS::expt::.has.correct.data")
test_that("everyting is not correct",
{
    expect_error(has.correct.data())
})

context("assignParamSettingsDS::expt::apds.init.coordinates.ratios")
test_that("everyting is incorrect",
{
  #settings and sharing should be null
  settings <- NULL
  sharing  <- NULL
  expect_equal(apds.init.coordinates.ratios(settings, sharing, param_names = c("first_var")), list())
  expect_equal(apds.init.coordinates.ratios(settings, sharing, param_names = c("first_var", "second_var")),list())
  expect_equal(apds.init.coordinates.ratios(settings, sharing, param_names = c("first_var", "second_var","third_var")),list())
})


options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 0)

context("assignParamSettingsDS::smk::not_allowed_sharing")
test_that("not allowed sharingt",
{
  expect_error(assignSharingSettingsDS())
  expect_equal(exists("settings_ds_share", where = 1), TRUE)
  expect_error(assignParamSettingsDS())
  expect_error(assignParamSettingsDS(123))
  expect_equal(are.params.created(param_names = c("first_var_X")), FALSE)
  expect_equal(are.params.created(param_names = c("first_var_X", "second_var_X")),FALSE)
  expect_equal(are.params.created(param_names = c("first_var_X", "second_var_X","third_var_X")),FALSE)
  expect_equal(are.params.created(param_names = c("first_var_X",1)), FALSE)
  expect_equal(are.params.created(param_names = c(1,2,4)),FALSE)
  expect_equal(are.params.created(param_names = "hello"),FALSE)
  expect_equal(are.params.created(param_names = TRUE),FALSE)
})


context("assignParamSettingsDS::expt::.are.params.created")
test_that("not allowed sharing",
{
  expect_equal(are.params.created(param_names = c(1)),FALSE)
  expect_equal(are.params.created(param_names = c("first_var_X")),FALSE)
  assign("first_var",1, pos=1)
  expect_equal(are.params.created(param_names = c("first_var", 1)),FALSE)
  expect_equal(are.params.created(param_names = c("first_var", "second_var_X")),FALSE)
  expect_equal(are.params.created(param_names = "hello"),FALSE)
  expect_equal(are.params.created(param_names = 1),FALSE)
  expect_equal(are.params.created(param_names = list()),FALSE)
})



options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 1)
assignSharingSettingsDS()

assign("first_var",1, pos=1)
assign("second_var",1, pos=1)
assign("third_var",1, pos=1)
assign("fourth_var",1, pos=1)


#("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

#("Step 2")
a <- getDataDS(master_mode =TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#("Step 3")
encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

#("step 4")
b <- getDataDS(master_mode =  FALSE)
rm(sharing,pos=1)
assign("sharing", master.1, pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
master.2 <- get("sharing",pos=1)


#("step 5")
decryptDataDS()
master.3 <- get("sharing",pos=1)


context("assignParamSettingsDS::smk::correct_outcome")
test_that("everything is correct",
{
  expect_equal(exists("settings_ds_share", where = 1), TRUE)
  expect_equal(exists("first_var", where = 1), TRUE)
  expect_equal(assignParamSettingsDS("first_var"),TRUE)
  expect_true(assignParamSettingsDS("first_var;second_var;third_var"))
  expect_true(assignParamSettingsDS("first_var;second_var;third_var;fourth_var"))

})


context("assignParamSettingsDS::expt::.init.coordinates.ratios")
test_that("everyting is correct",
{
  sharing <- apds.init.coordinates.ratios(get("settings_ds_share", pos = 1), get("sharing", pos = 1), param_names = c("first_var"))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 1)

  sharing <- apds.init.coordinates.ratios(get("settings_ds_share", pos = 1), get("sharing", pos = 1), param_names = c("first_var","second_var"))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 2)

  sharing <- apds.init.coordinates.ratios(get("settings_ds_share", pos = 1), get("sharing", pos = 1),
                                      param_names = c("first_var","second_var","third_var"))
  expect_equal("index_x" %in% names(sharing), TRUE)
  expect_equal("index_y" %in% names(sharing), TRUE)
  expect_equal("param_names" %in% names(sharing), TRUE)
  expect_equal(length(sharing$index_x), 3)


})


context("assignParamSettingsDS::expt::.create_vector")
test_that("correct and incorrect arguments",
{
   outcome <- create.vector(123)

   outcome <- create.vector("")
   expect_equal(length(outcome),0)

   outcome <- create.vector("a")
   expect_equal(length(outcome),1)

   outcome <-  outcome <- create.vector("a;b")
   expect_equal(length(outcome),2)

   outcome <- create.vector("a;b;c;d;e")
   expect_equal(length(outcome),5)


})

