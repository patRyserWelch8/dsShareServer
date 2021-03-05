
context("removeEncryptingDataDS::expt::no_settings")
test_that("no_setting",
{
  rm(list = ls(pos = 1), pos = 1)
  expect_false(removeEncryptingDataDS())
})

context("removeEncryptingDataDS::expt::settings_and_sharing")
{
  options(param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  assignSharingSettingsDS()
  settings <- get(".settings_ds_share", pos = 1)
  # needs to add elements to be removed.
  sharing <- list(rubbish = 1, rubbish_2 = 3)
  sharing[[settings$no_columns]] <- 1
  sharing[[settings$no_rows]] <- 1
  sharing[[settings$no_rows]] <- 1
  sharing[[settings$index_x]] <- 1
  sharing[[settings$index_y]] <- 1

  #master_mode
  sharing[[settings$data]] <- 1
  sharing[[settings$param_names]] <- 1

  expected.list   <- c(settings$no_columns, settings$no_rows, settings$index_x,
                       settings$index_y)
  expected.list  <- c(settings$data,settings$param_names, expected.list)
  expect_true(all(expected.list %in% names(sharing)))
  assign("sharing", sharing, pos = 1)
  expect_true(removeEncryptingDataDS(master_mode = TRUE))



  # needs to add elements to be removed.

  sharing <- list(rubbish = 1, rubbish_2 = 3)
  sharing[[settings$no_columns]] <- 1
  sharing[[settings$no_rows]] <- 1
  sharing[[settings$no_rows]] <- 1
  sharing[[settings$index_x]] <- 1
  sharing[[settings$index_y]] <- 1

  # NOT MASTER_MODE
  sharing[[settings$concealing]] <- 1
  assign("sharing", sharing, pos = 1)
  expect_true(removeEncryptingDataDS(master_mode = FALSE))
}

#"Step 0"
assignSharingSettingsDS()

#("Step 0")
rm(list=ls(pos = 1),pos=1)

#("Step 0")
options(param.name.struct = "sharing")
options(dsSS_sharing.allowed = 1)
assignSharingSettingsDS()




assign("pi_value_1", 100000, pos = 1)
assign("pi_value_2", 200000, pos = 1)
assign("pi_value_3", 300000, pos = 1)




#("Step 1")
encryptDataDS(TRUE, FALSE)
assign("master.1" ,get("sharing",pos=1), pos = 1)

#("Step 2")
assign("a", getDataDS(master_mode =TRUE), pos = 1)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
assign("receiver.1", get("sharing",pos=1), pos = 1)

#("Step 3")
encryptDataDS(FALSE, FALSE)
assign("receiver.2", get("sharing",pos=1), pos = 1)

#("step 4")
assign("b",getDataDS(master_mode =  FALSE), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("master.1", pos = 1), pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
assign("master.2", get("sharing",pos=1), pos = 1)


#("step 5")
decryptDataDS()
assign("master.3", get("sharing",pos=1), pos = 1)
outcome <- assignParamSettingsDS(c("pi_value_1","pi_value_2","pi_value_3"))
assign("master.3.5", get("sharing",pos = 1), pos = 1)
assign("f", getCoordinatesDS(), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("receiver.2", pos = 1), pos=1)
assignCoordinatesDS(f$header,payload = f$payload,f$property.a,f$property.b,f$property.c,f$property.d)
assign("receiver.2.5", get("sharing", pos = 1), pos = 1)
rm(sharing,pos=1)
assign("sharing", get("master.3.5", pos = 1), pos=1)

encryptParamDS()

assign("master.4", get("sharing",pos = 1), pos = 1)


context("removeEncryptingDataDS::expt::")
test_that("computations",
{
  sharing <- get("sharing", pos = 1)
  expect_equal(length(sharing),11)
  expect_equal(removeEncryptingDataDS(),TRUE)
  sharing <- get("sharing", pos = 1)

  expect_equal(length(sharing),6)
  expect_equal(all(c(settings$data,settings$no_columns, settings$no_rows,
                     settings$index_x, settings$index_y, settings$param_names) %in% names(sharing)), TRUE)

})



