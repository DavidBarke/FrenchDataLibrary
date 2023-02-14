#' @import purrr
#' @import dplyr
#' @import readr
#' @import stringr
NULL

is_column_names <- function(str) {
  stringr::str_count(str, ",") > 0
}

#' @export
read_factor_csv <- function(file) {
  lines <- readLines(file)

  empty_lines <- which(lines == "")
  data_skip <- empty_lines[-length(empty_lines)] %>% purrr::map_int(function(i) {
    if (is_column_names(lines[i + 1])) {
      i
    } else {
      i + 1
    }
  })
  data_end <- empty_lines[-1] - 2

  data_names <- lines[data_skip] %>% stringr::str_trim()
  data_names[1] <- "Monthly Factors"
  data <- purrr::map2(data_skip, data_end, function(skip, end) {
    readr::read_csv(
      file = file,
      skip = skip,
      n_max = end - skip
    )
  })
  names(data) <- data_names
  data
}

#' @export
process_factor_csv <- function(file) {
  lines <- readLines(file)

  empty_lines <- which(lines == "")
  data_skip <- empty_lines[-length(empty_lines)] %>% purrr::map_int(function(i) {
    if (is_column_names(lines[i + 1])) {
      i
    } else {
      i + 1
    }
  })
  data_end <- empty_lines[-1] - 2

  data_names <- lines[data_skip] %>% stringr::str_trim()
  data_names[1] <- "Monthly Factors"
  data <- purrr::map2(data_skip, data_end, function(skip, end) {
    readr::read_csv(
      file = file,
      skip = skip,
      n_max = end - skip
    ) %>%
      dplyr::rename(
        date = ...1, mkt_rf = `Mkt-RF`, smb = SMB, hml = HML, rf = RF
      )
  })
  names(data) <- data_names
  data
}

#' @export
process_portfolio_csv <- function(file) {
  lines <- readLines(file)

  data_start <- integer()
  data_end <- integer()
  for (i in seq_along(lines)) {
    if (lines[i] == "" && lines[i+1] == "") {
      if (length(data_start) > 0) {
        data_end <- c(data_end, i-2)
      }
      data_start <- c(data_start, i+2)
    }
  }
  data_end <- c(data_end, length(lines) - 2)

  data_names <- lines[data_start] %>% stringr::str_trim()
  data <- purrr::map2(data_start, data_end, function(start, end) {
    readr::read_csv(
      file = file,
      skip = start,
      n_max = end - start
    ) %>%
      dplyr::rename(date = ...1, Dec_1 = `Lo 10`, Dec_10 = `Hi 10`) %>%
      dplyr::rename_with(function(x) {
        stringr::str_replace(x, " ", "_") %>%
          stringr::str_to_lower()
      })
  })
  names(data) <- data_names
  data
}
