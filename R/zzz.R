

.onLoad <- function(libname, pkgname)
{
  if(is.null(getOption("dsSS_settings")))
  {
    options(dsSS_settings = ".settings_ds_share")
  }

  if(is.null(getOption("dsSS_param.name.struct")))
  {
    options(dsSS_param.name.struct = "sharing")
  }

  if(is.null(getOption("dsSS_sharing.allowed")))
  {
    options(dsSS_sharing.allowed = 0)
  }

  if(is.null(getOption("dsSS_sharing.near.equal.limit")))
  {
    options(dsSS_sharing.near.equal.limit = 1000000)
  }
}
