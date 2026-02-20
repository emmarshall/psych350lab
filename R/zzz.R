.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "psych350lab v", utils::packageVersion("psych350lab"),
    " - Functions for Research Methods and Statistical Analysis Lab Course"
  )
}
