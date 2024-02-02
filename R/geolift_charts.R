#' Draw a plotly test uplift chart
#'
#' @param geoLift Output from GeoLift::GeoLift()
#'
#' @return Plotly chart
#' @export
#'
#' @examples
geoLift_plotly_test_control <- function(geoLift, data_start, test_start, title = "", ytitle = "Visits"){

  treatment_obs <- as.data.frame(colMeans(matrix(geoLift$y_obs,
                                                 nrow = nrow(geoLift$test_id), ncol = geoLift$TreatmentEnd))) *
    nrow(geoLift$test_id)

  # Method to merge test region data copied directly from Meta GeoLift
  colnames(treatment_obs) <- c("t_obs")
  q_treatment_locations <- length(geoLift$test_id$name)

  df <- data.frame(t_obs = treatment_obs$t_obs, c_obs = geoLift$y_hat *
                     q_treatment_locations) |>
    dplyr::mutate(time = dplyr::row_number()) |>
    dplyr::mutate(date = as.Date(data_start) + dplyr::row_number() -1)


  title <- glue::glue("Test and Synthetic Control | {title}")

  test_line <- as.Date(test_start) -1

  df |>
    plotly::plot_ly() |>
    plotly::add_lines(y = ~c_obs, x = ~date, name = "Control",
                      line = list(color = itvPalette::itv_palette()$black,
                                  width = 1)) |>
    plotly::add_lines(y = ~t_obs, x = ~date, name = "Test",
                      line = list(color = itvPalette::itv_palette()$pink,
                                  width = 1)) |>
    plotly::add_segments(x = test_line, xend = test_line, y = 0, yend = ~max(t_obs),
                         showlegend = FALSE,
                         line = list(color = itvPalette::itv_palette()$black,
                                     width = 1, dash="dot")) |>
    plotly::layout(
      yaxis = list(
        title = ytitle
      ),
      xaxis = list(
        title = ""
      ),
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "left",  # use center of legend as anchor
                    x = 0),
      title = list(text = title, xanchor = "left", yanchor = "top", x = 0.1, y = 100),
      shapes = list(
        list(type = "rect", fillcolor = itvPalette::itv_palette()$blue, opacity = 0.05, x0 = test_line, x1 = max(df$date),
             y0 = 0, y1 = max(df$t_obs), line = list(color = itvPalette::itv_palette()$blue))
      ))
}

#' Draw a plotly test uplift chart
#'
#' @param geoLift Output from GeoLift::GeoLift()
#'
#' @return Plotly chart
#' @export
#'
#' @examples
geoLift_plotly_att <- function(geoLift, data_start, test_start, title = "", ytitle = "Visits"){
  plotly_data <- geoLift$summary$at |>
    dplyr::mutate(time = dplyr::row_number()) |>
    dplyr::mutate(date = as.Date(data_start) + dplyr::row_number() -1) |>
    dplyr::mutate(upper_bound = ifelse(is.na(upper_bound), Estimate, upper_bound),
                  lower_bound = ifelse(is.na(lower_bound), Estimate, lower_bound))

  test_line <- as.Date(test_start)

  title <- glue::glue("Uplift vs. Control | {title}")

  plotly_data |>
    plotly::plot_ly() |>
    plotly::add_lines(y = ~Estimate, x = ~date, name = "Uplift",
                      line = list(color = itvPalette::itv_palette()$black,
                                  width = 1)) |>
    plotly::add_trace(x = ~date, y = ~lower_bound,
      type = "scatter", mode = "lines",
      fillcolor = glue::glue("{itvPalette::itv_palette()$blue}33"),
      line = list(width = 0),
      showlegend = FALSE
      ) |>
    plotly::add_trace(x = ~date, y = ~upper_bound,
                      type = "scatter", mode = "lines",
                      fill = "tonexty",
                      fillcolor = glue::glue("{itvPalette::itv_palette()$blue}33"),
                      line = list(width = 0),
                      name = "90% Confidence Range") |>
    plotly::layout(
      yaxis = list(
        title = ytitle
      ),
      xaxis = list(
        title = ""
      ),
      legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "left",  # use center of legend as anchor
                             x = 0),
      title = list(text = title, xanchor = "left", yanchor = "top", x = 0.1, y = 100))
}
