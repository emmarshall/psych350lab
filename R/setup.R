# ============================================================================
#Install and load all dependencies for psych350lab
# ============================================================================

# All packages needed by psych350lab
# (Imports = required, Suggests = optional but recommended)
required_pkgs <- c(
  "dplyr", "tidyr", "haven", "here", "glue",
  "flextable", "officer", "ggplot2", "scales",
  "psych", "jmv", "tibble"
)

suggested_pkgs <- c(
  "knitr", "rmarkdown", "pkgdown",
  "webexercises", "tinytable", "htmltools",
  "testthat"
)

all_pkgs <- c(required_pkgs, suggested_pkgs)

# Install any missing packages
missing <- all_pkgs[!vapply(all_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  message("Installing ", length(missing), " missing package(s): ",
          paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All packages are already installed.")
}

# Load the required packages
invisible(lapply(required_pkgs, library, character.only = TRUE))

message("\nAll psych350lab dependencies are installed and loaded.")
message("You can now run: library(psych350lab)")

