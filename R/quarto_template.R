#' Creates a new GeoX project folder
#'
#' @param file_name directory name
#' @param ext_name template name
#'
#' @return NULL
#' @export
#'
#' @examples
#' # use_quarto_ext('my_new_project')
new_geoxR_project <- function(file_name = NULL,
                           ext_name = "quartotemplate") {

  # `file_name` will be the name of the sub-directory and new qmd report within it
  # `ext_name` needs to name the specific extension

  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }

  if(!dir.exists(file_name)) {
    dir.create(file_name)
  }

  # copy from internals
  file.copy(
    from = list.files(system.file(paste0("extdata/_extensions/", ext_name), package = "geoxR"), full.names = TRUE),
    to = file_name,
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )

}
