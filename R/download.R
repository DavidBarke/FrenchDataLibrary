#' Download file from French's website
#'
#' @import httr
#'
#' @param file File name on http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html.
#' @param path Path where extracted file is saved.
#' @param overwrite If `TRUE`, overwrite existing files.
#'
#' @export
download_french <- function(file, exdir = ".") {
  path <- tempfile()

  httr::GET(
    paste0(
      "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/",
      file,
      ".zip"
    ),
    httr::write_disk(path, overwrite = TRUE)
  )

  unzip(path, exdir = exdir, overwrite = TRUE)
}
