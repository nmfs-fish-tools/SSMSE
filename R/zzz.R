.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to SSMSE version ",
    utils::packageVersion(pkgname),
    " using Stock Synthesis 3.30.18."
  )
}
