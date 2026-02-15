.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "psychrmslab v", utils::packageVersion("psychrmslab"),
    " - Functions for Research Methods and Statistical Analysis Lab Course"
  )
}
