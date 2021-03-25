source('options/options_definitions.R')

context("assignSharingSettingsDS::expt::correct_outcome")
test_that("exists list",
{
   if (exists("settings_ds_share", envir = globalenv()))
   {
     rm("settings_ds_share", envir = globalenv())
   }
   assignSharingSettingsDS()
   settings.name <- getOption("dsSS_settings")
   print(settings.name)
   expect_equal(exists(settings.name, where = 1), TRUE)
})

test_that("correct fields",
{
   list.fields <- c("name.struct.sharing","name.struct.transfer", "masking", "concealing", "received", "encrypted", "decrypted",
                    "data", "index_x", "index_y", "no_columns", "no_rows", "min_rows","max_rows",
                    "min_columns", "max_columns", "min_value")

   settings <- get.settings(envir = globalenv())
   expect_equal(all(list.fields %in% names(settings)), TRUE)
})

test_that("with options",
{
   options(dsSS_sharing.allowed = 1)
   options(dsSS_sharing_param.name.struct = "sharing")
   options(dsSS_transfer.name.struct = "transfer")
   assignSharingSettingsDS()
   settings.name <- getOption("dsSS_settings")
   settings <- get(settings.name, pos = 1)

   expect_equal(settings$name.struct.sharing, getOption("dsSS_sharing_param.name.struct"))
   expect_equal(settings$name.struct.transfer, getOption("dsSS_transfer.name.struct"))
   expect_equal(settings$sharing.allowed, 1)

   options(dsSS_sharing.allowed = 0)
   set.default.options.restrictive()
   assignSharingSettingsDS()
   settings <- get(settings.name, pos=1)
   expect_equal(settings$name.struct.sharing, getOption("dsSS_sharing_param.name.struct"))
   expect_equal(settings$sharing.allowed, 0)
})

test_that("with options incorrect",
{
   set.default.options.incorrect.struct()
   assignSharingSettingsDS()
   settings.name <- getOption("dsSS_settings")
   settings <- get(settings.name, pos=1)
   expect_equal(settings$name.struct.sharing, "sharing")
   expect_equal(settings$sharing.allowed, 0)

   set.default.options.incorrect.allowed()
   assignSharingSettingsDS()
   settings <- get(settings.name, pos=1)
   expect_equal(settings$name.struct.sharing, "sharing")
   expect_equal(settings$sharing.allowed, 0)
})
