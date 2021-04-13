source("definition_tests/def_getEncodedDataDS.R")

rm(list=ls(pos = 1),pos=1)

options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 0)

context("getCoordinatesDS::expt::no_sharing_allowed")
test_that("no_sharing_allowed",
{
  expect_error(assignSharingSettingsDS())
  expect_error(getCoordinatesDS())
})

rm(list=ls(),pos=1)
options(param.name.struct = "sharing-test")
options(param.sharing.allowed = 1)

context("getCoordinatesDS::expt::no_sharing_structure")
test_that("no_sharing",
{
  expect_error(assignSharingSettingsDS())
  expect_error(getCoordinatesDS())

})

rm(list=ls(),pos=1)
options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 1)
assignSharingSettingsDS()


#print("Step 0")
assign("pi_value_1", 100000, pos = 1)
assign("pi_value_2", 200000, pos = 1)
assign("pi_value_3", 300000, pos = 1)
assignSharingSettingsDS()

#print("Step 1")
encryptDataDS(TRUE, FALSE)
master.1 <- get("sharing",pos=1)

#print("Step 2")
a <- getDataDS(master_mode = TRUE)
rm(sharing,pos=1)
assignDataDS(master_mode = FALSE,a$header,a$payload,a$property.a,a$property.b,a$property.c,a$property.d)
receiver.1 <- get("sharing",pos=1)

#print("Step 3")
encryptDataDS(FALSE, FALSE)
receiver.2 <- get("sharing",pos=1)

#print("step 4")
b <- getDataDS(master_mode =  FALSE)
rm(sharing,pos=1)
assign("sharing", master.1, pos=1)
assignDataDS(master_mode = TRUE, b$header,b$payload,b$property.a,b$property.b,b$property.c,b$property.d)
master.2 <- get("sharing",pos=1)
#print(names(master.2))

#print("step 5")
decryptDataDS()
master.3 <- get("sharing",pos=1)
#print(names(master.3))
assignParamSettingsDS(c("pi_value_1","pi_value_2","pi_value_3"))
master.3.5 <- get("sharing",pos=1)
#print(names(master.3.5))

context("getCoordinatesDS::expt::")
test_that("variables exists",
{
  data <- getCoordinatesDS()
  .test.data.structure(data)
  expect_equal(data$header,"FM1")
  .test.data.structure(data)
  .test.data.structure(encode.data.no.sharing())
  .test.data.structure(encode.data.with.sharing(master.1$encrypted,ncol(master.1$encrypted),15))
})

