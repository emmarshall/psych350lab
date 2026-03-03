# Declare global variables to avoid R CMD check NOTEs
utils::globalVariables(c(
":=",
  "cell", "condition", "freq", "iv1_label", "iv1_level",
  "iv2_label", "iv2_level", "n", "score", "sem",
  "subject", "variable", "iv", "dv", "group_label", "level_label", "level"
))

#' @importFrom stats sd cor.test aov pf as.formula complete.cases
NULL
