#' Title
#'
#' @param spots
#' @param n
#'
#' @return
#' @export
#'
#' @examples
get_large_spots <- function(spots, n = 10) {
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
    dplyr::mutate(date_time = lubridate::floor_date(lubridate::as_datetime(standard_datetime), 'minute')) |>
    dplyr::mutate(spot_id = dplyr::row_number()) |>
    dplyr::mutate(minute_times = purrr::map(
      .x = date_time,
      .f = ~ seq(.x - 10 * 60, length.out = 20, by = '1 min')
    )) |>
    dplyr::select(spot_id, minute_times) |>
    tidyr::unnest(minute_times) |>
    dplyr::rename(date_time = minute_times)

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
calc_spike_scale <- function(ga_minute, regions, large_spots) {

  # ----------------------------------------------------------------------------
  # Calculates the size of the minute level spike and returns a scaling factor and a plot
  # ----------------------------------------------------------------------------
  results_minute_match_agg <- ga_minute |>
    dplyr::filter(region %in% regions) |>
    dplyr::group_by(date, hour, minute, date_time) |>
    dplyr::summarise(sessions = sum(sessions))

  responses <- large_spots |>
    dplyr::left_join(results_minute_match_agg) |>
    dplyr::arrange(date_time) |>
    dplyr::group_by(spot_id) |>
    dplyr::mutate(minute_id = dplyr::row_number()) |>
    dplyr::mutate(sessions_pct = sessions / sum(sessions, na.rm = TRUE)) |>
    dplyr::group_by(minute_id) |>
    dplyr::summarise(sessions_pct = mean(sessions_pct, na.rm = TRUE))

  spike_scale = responses |>
    dplyr::mutate(pre_post = ifelse(minute_id < 10, "pre",
                             ifelse(minute_id %in% c(12, 13), "post", NA))) |>
    tidyr::drop_na() |>
    dplyr::group_by(pre_post) |>
    dplyr::summarise(sessions_pct = mean(sessions_pct)) |>
    tidyr::pivot_wider(names_from = pre_post, values_from = sessions_pct) |>
    dplyr::mutate(scale = post / pre) |>
    dplyr::pull(scale)

  plot <- plotly::plot_ly() |>
    plotly::add_lines(
      data = responses,
      x = ~ minute_id,
      y = ~ sessions_pct,
      mode = "lines",
      name = "Average Response",
      line = list(color = '#535353', width = 1)
    ) |>
    plotly::add_segments(
      x = 10,
      xend = 10,
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
get_adjustment_factor <-
  function(ga_minute, spots, test_region, control_region) {
    # ----------------------------------------------------------------------------
    # Calculates the size of the minute level spike and returns a scaling factor and a plot
    # ----------------------------------------------------------------------------

    large_spots <-  get_large_spots(spots)

    results_minute_match <- ga_minute |>
      regionalise_ga(test_region, control_region) |>
      dplyr::mutate(date_time = lubridate::as_datetime(glue::glue("{date} {hour}:{minute}:00")))

    spike_scale_test <-
      calc_spike_scale(results_minute_match, test_region, large_spots)
    spike_scale_control <-
      calc_spike_scale(results_minute_match, control_region, large_spots)

    return(list(
      spike_scale_test = spike_scale_test,
           spike_scale_control = spike_scale_control,
           spike_scale = spike_scale_control$spike_scale /  spike_scale_test$spike_scale)
    )
}
