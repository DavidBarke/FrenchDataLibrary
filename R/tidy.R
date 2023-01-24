#' @import lubridate
#'
#' @export
tidy_portfolio <- function(pf, factors) {
  pf %>%
    dplyr::inner_join(factors, by = "date") %>%
    dplyr::mutate(
      date = lubridate::ym(date),
      long_short = dec_10 - dec_1
    ) %>%
    dplyr::rename_with(
      .fn = function(x) paste0("D", stringr::str_extract(x, "\\d+")),
      .cols = tidyselect::starts_with("dec")
    ) %>%
    dplyr::rename(LS = long_short) %>%
    dplyr::select(
      date,
      tidyselect::starts_with("D", ignore.case = F),
      "LS", "rf", "smb", "hml", "mkt_rf"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("D", ignore.case = F),
        ~ .x - rf
      )
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        tidyselect::starts_with("D", ignore.case = F),
        "LS"
      ),
      names_to = "portfolio",
      values_to = "return"
    )
}
