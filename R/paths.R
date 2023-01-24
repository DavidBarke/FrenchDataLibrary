#' @export
data_paths <- function() {
  paths <- c(
    # Factors
    "F-F_Research_Data_Factors",
    "Portfolios_Formed_on_ME",
    "Portfolios_Formed_on_BE-ME",
    "Portfolios_Formed_on_OP",
    "Portfolios_Formed_on_INV",
    "Portfolios_Formed_on_E-P",
    "Portfolios_Formed_on_CF-P",
    "Portfolios_Formed_on_D-P",
    "Portfolios_Formed_on_AC",
    "Portfolios_Formed_on_BETA",
    "Portfolios_Formed_on_NI",
    "Portfolios_Formed_on_VAR",
    "Portfolios_Formed_on_RESVAR"
  ) %>%
    paste0("_CSV")
}
