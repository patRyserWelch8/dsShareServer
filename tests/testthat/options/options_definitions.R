# These function are required to test locally the use of options. Those
# are used
# Not recommended

set.default.options.not.restrictive <- function()
{
  options(sharing.near.equal.limit = 0.01)
  options(param.name.struct = "sharing_testing")
  options(sharing.allowed = 1)
}

set.default.options.restrictive <- function()
{
  options(param.name.struct = "sharing_testing")
  options(sharing.allowed = 0)
  options(sharing.near.equal.limit = 1000000)
}

set.default.options.incorrect.struct <- function()
{
  options(param.name.struct = "")
  options(param.sharing.allowed = 0)
}

set.default.options.numerical.struct <- function()
{
  options(param.name.struct = 1)
  options(param.sharing.allowed = 0)
}

set.default.options.incorrect.allowed <- function()
{
  options(param.name.struct = "sharing")
  options(param.sharing.allowed = 0.5)
}

remove.options <- function()
{
  .Options$sharing.near.equal.limit = NULL
  .Options$param.sharing.allowed = NULL
}

set.default.options.to.null <- function()
{
  options(dsShareServer.near.equal.limit = NULL)
}


set.allowed <- function()
{
  options(sharing.allowed = TRUE)
}

set.not.allowed <- function()
{
  options(sharing.allowed = 0)
}
