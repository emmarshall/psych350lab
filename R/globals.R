# Declare global variables to avoid R CMD check NOTEs
utils::globalVariables(c(
":=",
  "bg_label", "bg_level", "cell", "condition", "freq", "iv1_label", "iv1_level",
  "iv2_label", "iv2_level", "n", "score", "sem",
  "subject", "variable", "iv", "dv", "group_label", "level_label", "level", "id",
  "wg_label", "wg_level"
))


#' @importFrom stats setNames sd cor.test aov pf as.formula complete.cases
NULL
