
#' Load Plastics Data
#'
#' @returns A dataframe containing the plastics data from tidytuesday.
#' @export
#'
#' @examples
#' data <- load_data()
load_data <- function(){

  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv")

}
