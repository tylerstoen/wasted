#' Plot Plastic Waste vs Economic or Demographic Metric
#'
#' Renders a scatterplot of total recorded plastic waste against a chosen
#' country-level metric, colored by world region, with an optional linear
#' trend line.
#'
#' @param data A tibble returned by \code{load_data()}.
#' @param metric One of \code{gdp_per_capita} or \code{population}.
#'   Defaults to \code{gdp_per_capita}.
#' @param region Optional character string to filter to a single region.
#'   If NULL all regions are shown.
#' @param year Optional numeric year (2019 or 2020). If NULL both years
#'   are combined.
#' @param log_scale Logical. If TRUE (default), log-transforms both axes.
#'
#' @returns A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_x_continuous scale_color_brewer labs theme_light theme element_text element_blank scale_x_log10 scale_y_log10
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom scales label_dollar label_comma
#' @importFrom rlang enquo as_label
#'
#' @examples
#' data <- load_data()
#' plot_plastic_vs_metric(data)
#' plot_plastic_vs_metric(data, metric = "population", region = "Asia")
#' plot_plastic_vs_metric(data, metric = "gdp_per_capita", year = 2020)
plot_plastic_vs_metric <- function(data,
                                   metric    = gdp_per_capita,
                                   region    = NULL,
                                   year      = NULL,
                                   log_scale = TRUE) {

  metric_str <- rlang::as_label(rlang::enquo(metric))

  if (!metric_str %in% c("gdp_per_capita", "population")) {
    stop("'metric' must be gdp_per_capita or population.")
  }

  plot_data <- compute_country_stats(data, year = year) |>
    dplyr::filter(!is.na(region), total_plastic > 0)

  if (!is.null(region)) {
    plot_data <- plot_data |> dplyr::filter(region == !!region)
  }

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x     = {{ metric }},
      y     = total_plastic,
      color = region
    )) +
    ggplot2::geom_point(alpha = 0.8, size = 1.5) +
    ggplot2::geom_smooth(method = "lm", se = FALSE,
                         linewidth = 0.7, color = "black") +
    ggplot2::scale_color_brewer(palette = "Dark2", name = "World Region") +
    ggplot2::labs(
      x = if (metric_str == "gdp_per_capita") "GDP per Capita (USD)" else "Population",
      y = if (log_scale) "Log(Total Plastic Waste)" else "Total Plastic Waste"
    ) +
    ggplot2::theme_light(base_size = 14) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      axis.title       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  x_scale <- if (metric_str == "gdp_per_capita") {
    scales::label_dollar()
  } else {
    scales::label_comma()
  }

  if (log_scale) {
    p <- p + ggplot2::scale_y_log10() + ggplot2::scale_x_log10(labels = x_scale)
  } else {
    p <- p + ggplot2::scale_x_continuous(labels = x_scale)
  }

  p
}
