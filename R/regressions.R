#' @import parsnip
#' @import purrr
#'
#' @export
capm_regression <- function(pf) {
  pf %>%
    dplyr::group_by(portfolio) %>%
    summarise(
      model = parsnip::linear_reg() %>%
        parsnip::fit(return ~ mkt_rf, data = cur_data()) %>%
        `[[`("fit") %>%
        summary %>%
        list
    ) %>%
    dplyr::mutate(
      a = map_dbl(model, ~ .x$coefficients[1,1]),
      a_t = map_dbl(model, ~ .x$coefficients[1,3]),
      a_p = map_dbl(model, ~ .x$coefficients[1,4]),
      b = map_dbl(model, ~ .x$coefficients[2,1]),
      b_t = map_dbl(model, ~ .x$coefficients[2,3]),
      b_p = map_dbl(model, ~ .x$coefficients[2,4]),
      r_2 = map_dbl(model, ~ .x$r.squared)
    ) %>%
    dplyr::select(!model) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::starts_with("a"),
        tidyselect::starts_with("b"),
        "r_2"
      ),
      names_to = "statistic",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = "portfolio",
      values_from = "value"
    )
}

#' @export
three_factor_regression <- function(pf) {
  pf %>%
    dplyr::group_by(portfolio) %>%
    summarise(
      model = parsnip::linear_reg() %>%
        parsnip::fit(return ~ mkt_rf + smb + hml, data = cur_data()) %>%
        `[[`("fit") %>%
        summary %>%
        list
    ) %>%
    dplyr::mutate(
      a = map_dbl(model, ~ .x$coefficients[1,1]),
      a_t = map_dbl(model, ~ .x$coefficients[1,3]),
      a_p = map_dbl(model, ~ .x$coefficients[1,4]),
      b_mkt = map_dbl(model, ~ .x$coefficients[2,1]),
      b_mkt_t = map_dbl(model, ~ .x$coefficients[2,3]),
      b_mkt_p = map_dbl(model, ~ .x$coefficients[2,4]),
      b_smb = map_dbl(model, ~ .x$coefficients[3,1]),
      b_smb_t = map_dbl(model, ~ .x$coefficients[3,3]),
      b_smb_p = map_dbl(model, ~ .x$coefficients[3,4]),
      b_hml = map_dbl(model, ~ .x$coefficients[4,1]),
      b_hml_t = map_dbl(model, ~ .x$coefficients[4,3]),
      b_hml_p = map_dbl(model, ~ .x$coefficients[4,4]),
      r_2 = map_dbl(model, ~ .x$r.squared)
    ) %>%
    dplyr::select(!model) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::starts_with("a"),
        tidyselect::starts_with("b"),
        "r_2"
      ),
      names_to = "statistic",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = "portfolio",
      values_from = "value"
    )
}
