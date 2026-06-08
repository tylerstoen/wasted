#' Load Plastics Data
#'
#' Loads the full merged plastics dataset including country-level geographic
#' and economic data, pre-processed and bundled with the package.
#'
#' @returns A tibble containing the merged plastics dataset with country-level
#'   geographic, population, and GDP variables.
#' @export
#'
#' @importFrom arrow read_parquet
#'
#' @examples
#' data <- load_data()
#' head(data)
load_data <- function() {
  path <- system.file("extdata", "plastics_merged.parquet", package = "wasted")

  arrow::read_parquet(path)
}
