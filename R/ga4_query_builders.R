#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga4_filter_uk <- function(medium){
  googleAnalyticsR::ga_data_filter(country %contains% "United Kingdom")
}

#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga4_filter_medium <- function(medium){
    googleAnalyticsR::ga_data_filter(sessionMedium==!!medium & country %contains% "United Kingdom")
}

#' Title
#'
#' @param medium
#'
#' @return
#' @export
#'
#' @examples
ga4_filter_channel_group <- function(medium){
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
ga4_filter_cpc_brand <- function(brand_term){
    googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom")
}

#' Title
#'
#' @param brand_term
#'
#' @return
#' @export
#'
#' @examples
ga4_filter_cpc_generic <- function(brand_term){
    googleAnalyticsR::ga_data_filter(sessionMedium=="cpc" & !googleAdsKeyword %contains% brand_term & country %contains% "United Kingdom")
}

#' Title
#'
#' @param gender
#'
#' @return
#' @export
#'
#' @examples
ga4_filter_gender <- function(gender){
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
ga4_filter_age <- function(age_bracket){
    googleAnalyticsR::ga_data_filter(userAgeBracket %in% age_bracket & country %contains% "United Kingdom")
}
