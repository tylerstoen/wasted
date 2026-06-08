#' Plot plastic waste by company
#'
#' Creates a bar chart showing the companies with the highest
#' recorded plastic waste totals.
#'
#' @param data A data frame containing plastic waste data.
#' @param top_n Number of companies to display.
#'
#' @return A ggplot object.
#' @examples
#' data <- load_data()
#' plot_plastic_by_company(data)
#' plot_plastic_by_company(data, top_n = 5)
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_y_continuous labs theme_minimal
#' @importFrom scales label_comma
#' @importFrom stats reorder
#' @export
plot_plastic_by_company <- function(data, top_n = 10) {

  summary_df <- plastic_by_company(data, top_n)

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = stats::reorder(.data$parent_company, .data$total_plastic),
      y = total_plastic
    )
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma()
    ) +
    ggplot2::labs(
      title = "Top Companies by Recorded Plastic Waste",
      x = "",
      y = "Total Plastic Waste"
    ) +
    ggplot2::theme_minimal()
}
