#' Back up the current GeoxR working directory to Google Cloud
#'
#' @return
#' @export
#'
#' @examples
backup_to_cloud <- function(project_name = NULL, cloud_bucket = "itv_geox_data", filetypes = c(".*.rds$", ".*.R$", ".*.qmd$")){

  if(is.null(project_name)) stop("You must specify a project name")

  options("googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/devstorage.full_control")

  for(i in filetypes){
    files <- list.files(pattern = i)

    for(j in files){
      upload_path <- paste0(project_name, "/", j)
      googleCloudStorageR::gcs_upload(file = j, name = upload_path, bucket = cloud_bucket)
    }
  }
}
