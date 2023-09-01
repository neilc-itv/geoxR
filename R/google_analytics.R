#' Title
#'
#' @param ga_id
#' @param date_min
#' @param date_max
#' @param brand_term
#' @param conversion_goal
#' @param minute_level
#' @param page_path_contains
#'
#' @return
#' @export
#'
#' @examples
get_ga_all <-
  function(ga_id = NULL,
           date_min = NULL,
           date_max = NULL,
           brand_term = NULL,
           conversion_goal = NULL,
           minute_level = TRUE,
           page_path_contains = "/") {

    results_total <- ga_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::filter_clause_ga4(list(
        googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom"),
        googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains)
      ), "AND"),
      "sessions"
    )

    results_total_non_geo <- googleAnalyticsR::google_analytics(
      ga_id,
      metrics = "sessions",
      dimensions = c("date"),
      googleAnalyticsR::filter_clause_ga4(list(
        googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom"),
        googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains)
      ), "AND"),
      date_range = c(date_min, date_max),
      max = -1,
      anti_sample = TRUE)

    results_cpc <- ga_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::filter_clause_ga4(list(
        googleAnalyticsR::dim_filter("medium", "EXACT", "cpc"),
        googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains),
        googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
      ), "AND"),
      "sessions"
    )

    if(!is.null(brand_term)){
      results_cpc_brand <- ga_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::filter_clause_ga4(list(
          googleAnalyticsR::dim_filter("keyword", "PARTIAL", brand_term),
          googleAnalyticsR::dim_filter("medium", "EXACT", "cpc"),
          googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains),
          googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
        ), "AND"),
        "sessions"
      )

      results_cpc_generic <- ga_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::filter_clause_ga4(list(
          googleAnalyticsR::dim_filter("keyword", "PARTIAL", brand_term, not = TRUE),
          googleAnalyticsR::dim_filter("medium", "EXACT", "cpc"),
          googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains),
          googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
        ), "AND"),
        "sessions"
      )} else {
        results_cpc_brand <- NULL
        results_cpc_generic <- NULL
      }

    results_organic <- ga_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::filter_clause_ga4(list(
        googleAnalyticsR::dim_filter("medium", "EXACT", "organic"),
        googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains),
        googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
      ), "AND"),
      "sessions"
    )

    results_direct <- ga_query(
      ga_id,
      date_min,
      date_max,
      googleAnalyticsR::filter_clause_ga4(list(
        googleAnalyticsR::dim_filter("medium", "EXACT", "(none)"),
        googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains),
        googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
      ), "AND"),
      "sessions"
    )

    if(!is.null(conversion_goal)){
      results_conversion <- ga_query(
        ga_id,
        date_min,
        date_max,
        googleAnalyticsR::filter_clause_ga4(list(
          googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom"),
          googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains)
        ), "AND"),
        conversion_goal
      )} else {
        results_conversion <- NULL
      }

    if(minute_level){
        results_minute <- googleAnalyticsR::google_analytics(
          ga_id,
          date_range = c(date_min, date_max),
          metrics = "sessions",
          dimensions = c("date", "hour", "minute", "latitude", "longitude"),
          googleAnalyticsR::filter_clause_ga4(list(
            googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom"),
            googleAnalyticsR::dim_filter("pagePath", "PARTIAL", page_path_contains)
          ), "AND"),
          max = -1,
          anti_sample = TRUE
        )} else {
        results_minute <- NULL
    }

    list(
      aggregated = list(
        results_total = results_total,
        results_total_non_geo = results_total_non_geo,
        results_cpc = results_cpc,
        results_cpc_brand = results_cpc_brand,
        results_cpc_generic = results_cpc_generic,
        results_organic = results_organic,
        results_direct = results_direct,
        results_conversion = results_conversion
      ),
      granular = list(results_minute = results_minute)
    )
}


ga_query <- function(ga_id = NULL,
                     date_min = NULL,
                     date_max = NULL,
                     filter_clause = NULL,
                     metric = "sessions") {

    ga <- googleAnalyticsR::google_analytics(
      ga_id,
      metrics = metric,
      dimensions = c("date", "latitude", "longitude"),
      filter_clause,
      date_range = c(date_min, date_max),
      max = -1,
      anti_sample = TRUE
    ) |>
    dplyr::rename_with(~ "sessions", all_of(metric))

  ga
}
