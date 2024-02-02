#' Title
#'
#' @param ga
#' @param test_region
#' @param control_region
#' @param pre_start
#' @param pre_end
#' @param test_start
#' @param adjust_for_bleed_into_control
#' @param spike_adjustment
#'
#' @return
#' @export
#'
#' @examples
calculate_uplifts <-
  function(ga = NULL,
           test_region = NULL,
           control_region = NULL,
           pre_start = NULL,
           pre_end = NULL,
           test_start = NULL,
           adjust_for_bleed_into_control = TRUE,
           spike_adjustment = 0) {

    if(is.null(ga)) return(NULL)

    tryCatch({

      test_control_time_series_adjusted <- ga |>
        # filter(!glue::glue("{latitude},{longitude})") %in% excessive_latlong$latlong) |>
        dplyr::mutate(split = "all") |>
        calc_ga_uplifts(
          test_region = test_region,
          control_region = control_region,
          pre_start = pre_start,
          pre_end = pre_end,
          test_start = test_start
        ) |>
        dplyr::mutate(uplift_pct = test / control - 1) |>
        dplyr::ungroup() |>
        dplyr::mutate(control = ifelse(
          pre_test == "pre_period",
          control,
          ifelse(
            control < test,
            control - (test - control) * spike_adjustment,
            control
          )
        ))

      plot <-  plotly::plot_ly() |>
        plotly::add_lines(
          data = test_control_time_series_adjusted,
          x = ~ date,
          y = ~ control,
          mode = "lines",
          name = "Control",
          line = list(color = '#535353', width = 1)
        ) |>
        plotly::add_lines(
          data = test_control_time_series_adjusted,
          x = ~ date,
          y = ~ test,
          mode = "lines",
          name = "Test",
          line = list(color = '#a90061', width = 1)
        ) |>
        plotly::add_segments(
          x = test_start,
          xend = test_start,
          y = 0,
          yend = max(test_control_time_series_adjusted$test),
          line = list(color = "black", dash = "dash"),
          name = "Campaign Start"
        ) |>
        plotly::layout(xaxis = list(title = "Date"),
                       yaxis = list(title = "Sessions"))

      lift_summary <-  test_control_time_series_adjusted |>
        dplyr::filter(pre_test == "test_period") |>
        dplyr::group_by(pre_test) |>
        dplyr::summarise(control = sum(control),
                         test = sum(test)) |>
        dplyr::mutate(uplift_pct = round((test / control - 1) * 100, 1))

    },
    error=function(cond) {
      return(NULL)
    })

    return(
      list(
        time_series = test_control_time_series_adjusted,
        plot = plot,
        summary = lift_summary
      )
    )
  }
