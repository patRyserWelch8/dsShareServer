# These function are required to test locally the use of options. Those
# are used
# Not recommended

set.default.options.not.restrictive <- function()
{
  options(dsSS_sharing.near.equal.limit = 0.01)
  options(dsSS_param.name.struct = "sharing_testing")
  options(dsSS_sharing.allowed = 1)
  options(dsSS_settings = "settings_ds_share")
}

set.default.options.restrictive <- function()
{
  options(dsSS_param.name.struct = "sharing_testing")
  options(dsSS_sharing.allowed = 0)
  options(dsSS_sharing.near.equal.limit = 1000000)
  options(dsSS_settings = "settings_ds_share")
}

set.default.options.incorrect.struct <- function()
{
  options(dsSS_param.name.struct = "")
  options(dsSS_param.sharing.allowed = 0)
}

set.default.options.numerical.struct <- function()
{
  options(dsSS_param.name.struct = 1)
  options(dsSS_param.sharing.allowed = 0)
}

set.default.options.incorrect.allowed <- function()
{
  options(dsSS_param.name.struct = "sharing")
  options(dsSS_param.sharing.allowed = 0.5)
}

remove.options <- function()
{
  .Options$dsSS_sharing.near.equal.limit = NULL
  .Options$dsSS_param.sharing.allowed = NULL
}

set.default.options.to.null <- function()
{
  .Options$dsSS_sharing.near.equal.limit = NULL
}


set.allowed <- function()
{
  options(dsSS_sharing.allowed = TRUE)
}

set.not.allowed <- function()
{
  options(dsSS_sharing.allowed = 0)
}
