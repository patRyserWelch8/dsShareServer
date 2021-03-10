

.onLoad <- function(libname, pkgname)
{
  options(dsSS_settings = ".settings_ds_share")
  options(dsSS_param.name.struct = "sharing")
  options(dsSS_sharing.allowed = 1)
  options(dsSS_sharing.near.equal.limit = 1000000)
}
