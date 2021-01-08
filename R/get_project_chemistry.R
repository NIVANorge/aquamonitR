#' @title Get all water chemistry data for the specified project ID and date range.
#' @description Get all water chemistry data for the specified project ID and date range.
#'
#' @param proj_id \code{integer}: Project ID.
#' @param st_dt \code{character}: Start of period of interest in format 'dd.mm.yyyy'.
#' @param end_dt \code{character}: End of period of interest in format 'dd.mm.yyyy'.
#' @param token \code{character}: Valid API access token. If \code{NULL}, will first attempt to read credentials from a '.auth' file in the installation folder. If this fails, will prompt for username and password.
#' @param na.rm \code{logical}: Should missing values be removed? Defaults to \code{TRUE}.
#'
#' @return \code{list}: A data.frame.
#' @export
get_project_chemistry <- function(proj_id, st_dt, end_dt, token = NULL, na.rm = TRUE) {

  where <- paste0("project_id = ", proj_id, " and sample_date >= ", st_dt, " and sample_date <= ", end_dt)

  table <- "water_chemistry_output"

  qry <- .query(where = where, token = token)

  result <- qry$map(table)

  cache_site <- .get_cache_site()

  df_list <- list()

  for (page in 0:(result$Pages - 1)) {

    url <- paste(cache_site, "query", qry$key, table, page, sep = "/")

    response <- get_json(qry$token, url)

    df_list[[page + 1]] <- response$Items
  }

  df <- do.call("rbind", df_list)

  if (na.rm) {

    df <- df[!is.na(df$Value), ]

  }

  df <- df[, !(names(df) %in% c("$type", "Sample.$type", "Parameter.Id", "Sample.Id"))]

  df$Sample.SampleDate <- as.POSIXct(df$Sample.SampleDate, format = "%Y-%m-%dT%H:%M:%SZ")

  # if (!("Sample.Depth1" %in% colnames(df))) {
  #
  #   df$Sample.Depth1 <- NA_real_
  #
  # }
  #
  # if (!("Sample.Depth2" %in% colnames(df))) {
  #
  #   df$Sample.Depth2 <- NA_real_
  #
  # }

  df <- df[, c("Sample.Station.Project.Id", "Sample.Station.Project.Name", "Sample.Station.Id", "Sample.Station.Code", "Sample.Station.Name", "Sample.SampleDate", "Sample.Depth1", "Sample.Depth2", "Parameter.Name", "Flag", "Value", "Parameter.Unit")]

  names(df) <- c("ProjectId", "ProjectName", "StationId", "StationCode", "StationName", "SampleDate", "Depth1", "Depth2", "ParameterName", "Flag", "Value", "Unit")

  df

}
