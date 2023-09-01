#' Title
#'
#' @param spots
#' @param n
#'
#' @return
#' @export
#'
#' @examples
get_large_spots_ga4 <- function(spots, n = 10) {
  # ----------------------------------------------------------------------------
  # finds large spots from a baRb::barb_get_spots() call and formats for match to google analytics
  # ----------------------------------------------------------------------------

  large_spots_10 <- spots |>
    dplyr::mutate(
      hour = lubridate::hour(standard_datetime),
      minute = lubridate::minute(standard_datetime)
    ) |>
    dplyr::top_n(n, all_adults)

  large_spots <- large_spots_10 |>
    dplyr::mutate(date_time = lubridate::floor_date(lubridate::as_datetime(standard_datetime), 'hour')) |>
    dplyr::mutate(spot_id = dplyr::row_number()) |>
    dplyr::mutate(hour_times = purrr::map(
      .x = date_time,
      .f = ~ seq(.x - 2 * 60 * 60, length.out = 4, by = '1 hour')
    )) |>
    dplyr::select(spot_id, hour_times) |>
    tidyr::unnest(hour_times) |>
    dplyr::rename(date_time = hour_times)

  large_spots
}

#' Title
#'
#' @param ga_minute
#' @param regions
#' @param large_spots
#'
#' @return
#' @export
#'
#' @examples
calc_spike_scale_ga4 <- function(ga_hour, regions, large_spots) {

  # ----------------------------------------------------------------------------
  # Calculates the size of the minute level spike and returns a scaling factor and a plot
  # ----------------------------------------------------------------------------
  results_hour_match_agg <- ga_hour |>
    dplyr::filter(region %in% regions) |>
    dplyr::group_by(date, hour, date_time) |>
    dplyr::summarise(sessions = sum(sessions))

  responses <- large_spots |>
    dplyr::left_join(results_hour_match_agg) |>
    dplyr::arrange(date_time) |>
    dplyr::group_by(spot_id) |>
    dplyr::mutate(hour_id = dplyr::row_number()) |>
    dplyr::mutate(sessions_pct = sessions / sum(sessions, na.rm = TRUE)) |>
    dplyr::group_by(hour_id) |>
    dplyr::summarise(sessions_pct = mean(sessions_pct, na.rm = TRUE))

  spike_scale = responses |>
    dplyr::mutate(pre_post = ifelse(hour_id < 3, "pre",
                             ifelse(hour_id %in% c(3), "post", NA))) |>
    tidyr::drop_na() |>
    dplyr::group_by(pre_post) |>
    dplyr::summarise(sessions_pct = mean(sessions_pct)) |>
    tidyr::pivot_wider(names_from = pre_post, values_from = sessions_pct) |>
    dplyr::mutate(scale = post / pre) |>
    dplyr::pull(scale)

  plot <- plotly::plot_ly() |>
    plotly::add_lines(
      data = responses,
      x = ~ hour_id,
      y = ~ sessions_pct,
      mode = "lines",
      name = "Average Response",
      line = list(color = '#535353', width = 1)
    ) |>
    plotly::add_segments(
      x = 2.5,
      xend = 2.5,
      y = 0,
      yend = 0.3,
      line = list(color = "black", dash = "dash"),
      name = "Spot Broadcast Time"
    ) |>
    plotly::layout(xaxis = list(title = "Minute"),
                   yaxis = list(title = "Sessions Percent"))

  return(list(spike_scale = spike_scale, spike_plot = plot))
}

#' Title
#'
#' @param ga_minute
#' @param spots
#' @param test_region
#' @param control_region
#'
#' @return
#' @export
#'
#' @examples
get_adjustment_factor_ga4 <-
  function(ga_hour, spots, test_region, control_region) {
    # ----------------------------------------------------------------------------
    # Calculates the size of the minute level spike and returns a scaling factor and a plot
    # ----------------------------------------------------------------------------

    large_spots <-  get_large_spots_ga4(spots)

    results_hour_match <- ga_hour |>
      regionalise_ga(test_region, control_region) |>
      dplyr::mutate(date_time = lubridate::as_datetime(glue::glue("{date} {hour}:00:00")))

    spike_scale_test <-
      calc_spike_scale_ga4(results_hour_match, test_region, large_spots)
    spike_scale_control <-
      calc_spike_scale_ga4(results_hour_match, control_region, large_spots)

    return(list(
      spike_scale_test = spike_scale_test,
           spike_scale_control = spike_scale_control,
           spike_scale = spike_scale_control$spike_scale /  spike_scale_test$spike_scale)
    )
}
