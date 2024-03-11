#' Title
#'
#' @param ga_id
#' @param date_min
#' @param date_max
#' @param filter_clause
#' @param metric
#' @param scale_to_national
#'
#' @return
#' @export
#'
#' @examples
ga_query <- function(ga_id = NULL,
                     date_min = NULL,
                     date_max = NULL,
                     filter_clause = NULL,
                     metric = "sessions",
                     scale_to_national = TRUE) {

    ga <- googleAnalyticsR::google_analytics(
      ga_id,
      metrics = metric,
      dimensions = c("date", "city"),
      date_range = c(date_min, date_max),
      dim_filters = filter_clause,
      max = -1,
      anti_sample = TRUE
    ) |>
    dplyr::rename_with(~ "sessions", tidyselect::all_of(metric))

    if(scale_to_national){
      ga_national <- googleAnalyticsR::google_analytics(
        ga_id,
        metrics = metric,
        date_range = c(date_min, date_max),
        dim_filters = filter_clause,
        max = -1,
        anti_sample = TRUE
      ) |>
        dplyr::rename_with(~ "sessions", tidyselect::all_of(metric))

      scaling_factor <- sum(ga_national$sessions) / sum(ga$sessions)

      ga$sessions <- ga$sessions * scaling_factor
    }

    # Verify returned traffic
    requested_dates <- tibble::tibble(
      date_requested = seq.Date(as.Date(date_min), as.Date(date_max), "day")
    )

    ga_check <- ga |>
      dplyr::group_by(date) |>
      dplyr::summarise(sessions = sum(sessions))

    check <- requested_dates |>
      dplyr::left_join(ga_check, by = c("date_requested"="date")) |>
      dplyr::filter(is.na(sessions))

    if(nrow(check) > 0){
      warning(glue::glue("Warning: no traffic on date: {check$date_requested}"))
    }

  ga
}
