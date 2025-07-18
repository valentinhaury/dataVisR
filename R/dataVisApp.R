#' dataVisR
#'
#' Creates a Shiny app that displays for each variable in the given data frame:
#' - Summary statistics
#' - Min/Max (for numeric variables)
#' - Median or "No median" (if no repeating values)
#' - Histogram (small variance of Integers) or Density plot (numeric) or barplot (categorical)
#'
#' @param df A data frame to visualize.
#' @return starts a shiny-app (no return value)
#' @importFrom shiny shinyApp fluidPage tabPanel
#' @importFrom ggplot2 ggplot aes geom_density geom_bar labs theme element_text theme_minimal element_rect element_line
#' @importFrom bslib bs_theme font_google
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   dataVis(mtcars)
#' }
dataVis <- function(df) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      theme = app_theme(),
      shiny::titlePanel(paste("Variables of dataframe:", deparse(substitute(df)))),
      if (length(names(df)) > 0) {
        do.call(shiny::tabsetPanel, lapply(names(df), function(var) {
          shiny::tabPanel(var,
                          shiny::h3(paste("Variable:", var)),
                          shiny::verbatimTextOutput(paste0("extra_", var)),
                          shiny::plotOutput(paste0("plot_", var))
          )
        }))
      } else {
        shiny::h3("The data frame has no variables.")
      }
    ),
    server = function(input, output, session) {
      for (var in names(df)) {
        local({
          v <- var

          output[[paste0("extra_", v)]] <- shiny::renderPrint({
            x <- df[[v]]
            if (is.numeric(x)) {
              cat("Min:", min(x, na.rm = TRUE), "\n")
              cat("Max:", max(x, na.rm = TRUE), "\n")
              cat("Median:", median(x, na.rm = TRUE), "\n")
            } else {
              freq <- sort(table(x), decreasing = TRUE)
              if (length(freq) == 0) {
                cat("No values.\n")
              } else if (freq[1] > 1) {
                cat("Median (most frequent value):", names(freq[1]), "\n")
              } else {
                cat("No median\n")
              }
            }
          })

          output[[paste0("plot_", v)]] <- shiny::renderPlot({
            x <- df[[v]]
            if (is.numeric(x)) {
              unique_vals <- unique(na.omit(x))
              is_integer_like <- all(unique_vals == floor(unique_vals))
              if (length(unique_vals) < 20 && is_integer_like) {
                # histogram for small number of integers
                ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
                  ggplot2::geom_histogram(binwidth = 1, fill = "#657C6A", color = "#BB3E00") +
                  ggplot2::labs(title = paste("Histogram of", v), x = v, y = "Count") +
                  plot_colorscheme()
              } else {
                # density for all other numeric values
                ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
                  ggplot2::geom_density(fill = "#657C6A", alpha = 0.5, color = "#BB3E00") +
                  ggplot2::labs(title = paste("Density of", v), x = v, y = "Density") +
                  plot_colorscheme()
              }
            } else {
              # bar plot for categorical values
              ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
                ggplot2::geom_bar(fill = "#657C6A", color = "#BB3E00") +
                ggplot2::labs(title = paste("Counts of", v), x = v, y = "Count") +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                plot_colorscheme()
            }
          })
        })
      }
    }
  )
}

app_theme <- function() {
  bslib::bs_theme(
    bg = "#A2B9A7",
    fg = "#657C6A",
    primary = "#BB3E00",
    base_font = bslib::font_google("Raleway")
  )
}

plot_colorscheme <- function() {
  ggplot2::theme_minimal(base_family = "Raleway") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#A2B9A7", color = NA),
      plot.background = ggplot2::element_rect(fill = "#A2B9A7", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#657C6A", size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#657C6A", size = 12),
      axis.title = ggplot2::element_text(color = "#657C6A", size = 14),
      plot.title = ggplot2::element_text(color = "#BB3E00", size = 18, face = "bold"),
      legend.background = ggplot2::element_rect(fill = "#A2B9A7", color = NA),
      legend.key = ggplot2::element_rect(fill = "#A2B9A7"),
      legend.title = ggplot2::element_text(color = "#657C6A"),
      legend.text = ggplot2::element_text(color = "#657C6A")
    )
}
