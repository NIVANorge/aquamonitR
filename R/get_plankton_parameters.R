#' @title Get all plankton parameter data for the specified project ID and date range.
#' @description Get all plankton parameter data for the specified project ID and date range.
#'
#' @param proj_id \code{integer}: Project ID.
#' @param st_dt \code{character}: Start of period of interest in format 'dd.mm.yyyy'.
#' @param end_dt \code{character}: End of period of interest in format 'dd.mm.yyyy'.
#' @param token \code{character}: Valid API access token. If \code{NULL}, will prompt for username and password.
#' @param na.rm \code{logical}: Should missing values be removed? Defaults to \code{TRUE}.
#'
#' @return \code{list}: A data.frame.
#' @export
get_plankton_parameters <- function(proj_id, st_dt, end_dt, token = NULL, na.rm = TRUE) {

  if (is.null(token)) {

    token <- login()

  }

  where <- paste0("datatype = Plankton and project_id = ", proj_id, " and sample_date>=", st_dt, " and sample_date<=", end_dt)

  table <- "plankton_parameters"

  qry <- .query(where = where, token = token)

  result <- qry$map(table)

  cache_site <- .get_cache_site()

  df_list <- list()

  for (page in 0:(result$Pages - 1)) {

    url <- paste(cache_site, "query", qry$key, table, page, sep = "/")

    response <- get_json(qry$token, url)

    df_list[[page + 1]] <- response$Items

  }

  df <- do.call(dplyr::bind_rows, df_list)

  if (na.rm) {

    df <- df[!is.na(df$Value), ]

  }

  df <- df[, !(names(df) %in% c("$type", "Parameter.Id", "Parameter.Datatype", "Parameter.Sort", "Sample.Id", "Category.Id"))]

  df$Sample.SampleDate <- as.POSIXct(df$Sample.SampleDate, format = "%Y-%m-%dT%H:%M:%SZ")
  df$SampleDate <- as.Date(df$Sample.SampleDate)

  df <- df[, c("Sample.Station.Project.Id", "Sample.Station.Project.Name", "Sample.Station.Id", "Sample.Station.Code", "Sample.Station.Name", "SampleDate", "Sample.Depth1", "Sample.Depth2", "Category.Name", "Parameter.Name", "Value", "Parameter.Unit")]
  names(df) <- c("project_id", "project_name", "station_id", "station_code", "station_name", "date", "depth_1", "depth_2", "group", "parameter", "value", "unit")
  df
}
