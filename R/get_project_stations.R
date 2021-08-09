#' @title Get stations associated with a specific project.
#' @description Get stations associated with a specific project.
#'
#' @param proj_id \code{integer}: Project ID.
#' @param token \code{character}: Valid API access token. If \code{NULL}, will first attempt to read credentials from a '.auth' file in the installation folder. If this fails, will prompt for username and password.
#'
#' @return \code{list}: A data.frame.
#' @export
get_project_stations <- function(proj_id, token = NULL) {

  if (is.null(token)) {

    token <- login()

  }

  aqua_site <- .get_aqua_site()

  url <- paste0(aqua_site, "/api/projects/", proj_id, "/stations")

  df <- get_json(token, url)

  df <- df[, c("Project.Id", "Id", "Code", "Name", "Type.Text")]

  names(df) <- c("project_id", "station_id", "station_code", "station_name", "type")

  df

}
