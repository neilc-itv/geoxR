#' Back up the current GeoxR working directory to Google Cloud
#'
#' @return
#' @export
#'
#' @examples
backup_to_cloud <- function(project_name = NULL, cloud_bucket = "itv_geox_data", file_types = c(".rds", ".qmd")){

  options("googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/devstorage.full_control")

  googleCloudStorageR::gcs_upload("R/cloud_backup.R", bucket = cloud_bucket)

  if(is.null(project_name)) stop("You must specify a project name.")


  "https://console.cloud.google.com/storage/browser/itv_geox_data"
}
