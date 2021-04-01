# The function has now been rename assignVariable .... test needs to be renamed and extended.
context("assignTransferSettingsDS::expt::no_settings")
test_that("no_settings",
{
  rm(list=ls(pos = 1), pos = 1)
  expect_error(assignTransferSettingsDS())
})


options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 0)
assignSharingSettingsDS()


context("assignTransferSettingsDS::expt::settings_not_allowed")
test_that("settings",
{
  expect_error(assignTransferSettingsDS())
})

options(dsSS_sharing_param.name.struct = "sharing")
options(dsSS_sharing.allowed = 1)
assignSharingSettingsDS()


context("assignTransferSettingsDS::expt::settings_allowed")
test_that("settings_no_sharing",
{
  expect_error(assignTransferSettingsDS())
  expect_true(assignTransferSettingsDS(1))
  expect_true(assignTransferSettingsDS(2))
})
