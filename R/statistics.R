skewness <- function(x) {
  # g1 definition from Wikipedia
  n <- length(x)
  x_c <- x - mean(x)

  (1/n * sum(x_c^3)) /
    (1/n * sum(x_c^2))^(3/2)
}

kurtosis <- function(x) {
  # g2 definition from Wikipedia
  n <- length(x)
  x_c <- x - mean(x)

  (1/n * sum(x_c^4)) /
    (1/n * sum(x_c^2))^2 - 3
}

#' @export
portfolio_stats <- function(pf) {
  n <- nrow(pf %>% filter(portfolio == "D1"))

  pf %>%
    dplyr::group_by(portfolio) %>%
    dplyr::summarise(
      mean = mean(return),
      sd = sd(return),
      skewness = skewness(return),
      kurtosis = kurtosis(return)
    ) %>%
    dplyr::mutate(
      t_test = mean / (sd / sqrt(n)),
      p_value = 2 * (1 - pt(abs(t_test), df = n - 1))
    ) %>%
    tidyr::pivot_longer(
      cols = c("mean", "sd", "skewness", "kurtosis", "t_test", "p_value"),
      names_to = "statistic",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = "portfolio",
      values_from = "value"
    ) %>%
    dplyr::select(c("statistic", paste0("D", 1:10), "LS"))
}
