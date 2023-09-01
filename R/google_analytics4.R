#' Title
#'
#' @param ga_id
#' @param date_min
#' @param date_max
#' @param brand_term
#' @param conversion_goal
#' @param minute_level
#'
#' @return
#' @export
#'
#' @examples
get_ga4_all <-
  function(ga_id = NULL,
           date_min = NULL,
           date_max = NULL,
           brand_term = NULL,
           conversion_goal = NULL,
           hour_level = TRUE) {

    results_total <- ga4_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::ga_data_filter(country %contains% "United Kingdom"),
      "sessions"
    )

    results_cpc <- ga4_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & country %contains% "United Kingdom"),
      "sessions"
    )

    if(!is.null(brand_term)){
      results_cpc_brand <- ga4_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom"),
        "sessions"
      )

      results_cpc_generic <- ga4_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & !googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom"),
        "sessions"
      )} else {
        results_cpc_brand <- NULL
        results_cpc_generic <- NULL
      }

    results_organic <- ga4_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::ga_data_filter(sessionMedium=="organic" & country %contains% "United Kingdom"),
      "sessions"
    )

    results_direct <- ga4_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::ga_data_filter(sessionMedium=="(none)" & country %contains% "United Kingdom"),
      "sessions"
    )

    if(!is.null(conversion_goal)){
      results_conversion <- ga4_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::ga_data_filter(sessionMedium=="(none)" & country %contains% "United Kingdom"),
        conversion_goal
      )} else {
        results_conversion <- NULL
      }

    if(hour_level){
        results_hour <- googleAnalyticsR::ga_data(
          ga_id,
          metrics = "sessions",
          dimensions = c("date", "hour", "city"),
          date_range = c(date_min, date_max),
          googleAnalyticsR::ga_data_filter(country %contains% "United Kingdom"),
          limit = -1,
        )
      } else {
        results_hour <- NULL
    }

    list(
      aggregated = list(
        results_total = results_total,
        results_cpc = results_cpc,
        results_cpc_brand = results_cpc_brand,
        results_cpc_generic = results_cpc_generic,
        results_organic = results_organic,
        results_direct = results_direct,
        results_conversion = results_conversion
      ),
      granular = list(results_hour = results_hour)
    )
}


ga4_query <- function(ga_id = NULL,
                     date_min = NULL,
                     date_max = NULL,
                     filter_clause = NULL,
                     metric = "sessions",
                     is_GA4 = TRUE) {

    ga <- googleAnalyticsR::ga_data(
      ga_id,
      metrics = metric,
      dimensions = c("date", "city"),
      date_range = c(date_min, date_max),
      dim_filters = filter_clause,
      limit = -1
    ) |>
    dplyr::rename_with(~ "sessions", all_of(metric))

  ga
}
