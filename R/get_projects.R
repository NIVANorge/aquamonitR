#' @title Get full list of projects in the Nivadatabase/Aquamonitor.
#' @description Get full list of projects in the Nivadatabase/Aquamonitor.
#'
#' @param token \code{character}: Valid API access token. If \code{NULL}, will first attempt to read credentials from a '.auth' file in the installation folder. If this fails, will prompt for username and password.
#'
#' @return \code{list}: A data.frame.
#' @export
get_projects <- function(token = NULL) {

  if (is.null(token)) {

    token <- login()

  }

  aqua_site <- .get_aqua_site()

  url <- paste0(aqua_site, "/api/Projects")

  df <- get_json(token, url)

  df <- df[, c("Id", "Number", "Name", "Description")]

  names(df) <- c("project_id", "project_code", "project_name", "description")

  df

}
