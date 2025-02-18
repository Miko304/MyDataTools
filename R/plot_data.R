#' Plot dataset
#'
#' This function plots data from a data frame using ggplot2.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x The name of the column to be used for the x-axis.
#' @param y The name of the column to be used for the y-axis (optional for histogram and bar plot).
#' @param plot_type The type of plot ("line", "scatter", "histogram", "bar").
#' @return A ggplot object.
#' @export
#' @examples
#' library(ggplot2)
#' data <- mtcars
#'
#' # Example using mtcars dataset for line plot
#' p <- plot_data(data, "cyl", "mpg", "line")
#' print(p)
#'
#' # Example using mtcars dataset for scatter plot
#' p <- plot_data(data, "cyl", "mpg", "scatter")
#' print(p)
#'
#' # Example using mtcars dataset for histogram
#' p <- plot_data(data, "mpg", plot_type = "histogram")
#' print(p)
#'
#' # Example using mtcars dataset for bar plot
#' p <- plot_data(data, "cyl", plot_type = "bar")
#' print(p)
#'
#' # Example using mtcars dataset for bar plot with y
#' p <- plot_data(data, "cyl", "mpg", "bar")
#' print(p)

plot_data <- function(data, x, y = NULL, plot_type = "line") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed.")
  }

  if (!x %in% names(data)) {
    stop(paste("Column", x, "not found in data."))
  }

  if (!is.null(y) && !y %in% names(data)) {
    stop(paste("Column", y, "not found in data."))
  }

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

  if (plot_type == "line") {
    p <- p + ggplot2::geom_line()
  } else if (plot_type == "scatter") {
    p <- p + ggplot2::geom_point()
  } else if (plot_type == "histogram") {
    if (!is.null(y)) {
      warning("Ignoring y parameter for histogram plot type.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x)) + ggplot2::geom_histogram()
  } else if (plot_type == "bar") {
    if (is.null(y)) {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x)) + ggplot2::geom_bar()
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) + ggplot2::geom_bar(stat = "identity")
    }
  } else {
    stop("Unsupported plot type")
  }

  return(p)
}
