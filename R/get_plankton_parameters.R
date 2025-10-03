#' @title Get Plankton Parameters
#' @description Get Plankton Parameters and their descriptions from AquaMonitor
#'
#' @param name \code{character}: Optional parameter name to filter results. Defaults to \code{NULL}, which retrieves all parameters.
#' @param token \code{character}: Valid API access token. If \code{NULL}, will prompt for username and password.
#' @return \code{data.frame}: A data.frame with columns: id, parameter, unit, sort.
#' @export
get_plankton_parameters <- function(name = NULL, token = NULL) {
  if (is.null(token)) {

    token <- login()
  }

  if (is.null(name)) {
    name <- "%"
  }

  path <- paste0(.get_aqua_site(), "/api/query/parameter?datatype=Plankton&name=", name)
  response <- get_json(token, path)
  df <- response[order(response$Sort),c("Id", "Name", "Unit", "Sort")]
  names(df) <- c("id", "parameter", "unit", "sort")

  df
}

