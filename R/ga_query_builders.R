#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga_filter_uk <- function(medium, ga4 = TRUE){
  if(ga4){
    googleAnalyticsR::ga_data_filter(country %contains% "United Kingdom")
  } else {
    googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
  }
}

#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga_filter_medium <- function(medium, ga4 = TRUE){
  if(ga4){
    googleAnalyticsR::ga_data_filter(sessionMedium==!!medium & country %contains% "United Kingdom")
  } else {
    googleAnalyticsR::filter_clause_ga4(list(
      googleAnalyticsR::dim_filter("medium", "EXACT", medium),
      googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
    ), "AND")
  }
}

#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga_filter_channel_group <- function(medium){
  googleAnalyticsR::ga_data_filter(sessionDefaultChannelGrouping==!!medium & country %contains% "United Kingdom")
}

#' Title
#'
#' @param brand_term
#'
#' @return
#' @export
#'
#' @examples
ga_filter_cpc_brand <- function(brand_term, ga4 = TRUE){
  if(ga4){
    googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom")
  } else {
    googleAnalyticsR::filter_clause_ga4(list(
      googleAnalyticsR::dim_filter("keyword", "PARTIAL", brand_term),
      googleAnalyticsR::dim_filter("medium", "EXACT", "cpc"),
      googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
    ), "AND")
  }
}

#' Title
#'
#' @param brand_term
#'
#' @return
#' @export
#'
#' @examples
ga_filter_cpc_generic <- function(brand_term, ga4 = TRUE){
  if(ga4){
    googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & !googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom")
  } else {
    googleAnalyticsR::filter_clause_ga4(list(
      googleAnalyticsR::dim_filter("keyword", "PARTIAL", brand_term, not = TRUE),
      googleAnalyticsR::dim_filter("medium", "EXACT", "cpc"),
      googleAnalyticsR::dim_filter("country", "EXACT", "United Kingdom")
    ), "AND")
  }
}

#' Title
#'
#' @param gender
#'
#' @return
#' @export
#'
#' @examples
ga_filter_gender <- function(gender){
    googleAnalyticsR::ga_data_filter(userGender==gender & country %contains% "United Kingdom")
}

#' Title
#'
#' @param age_bracket
#'
#' @return
#' @export
#'
#' @examples
ga_filter_age <- function(age_bracket){
    googleAnalyticsR::ga_data_filter(userAgeBracket %in% age_bracket & country %contains% "United Kingdom")
}
