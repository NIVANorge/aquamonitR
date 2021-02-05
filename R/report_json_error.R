.report_json_error <- function(response) {

  msg <- paste0("AquaMonitor failed with status: ", response$status_code, " and message:")

  if (!is.null(response$text)) {

    msg <- paste0(msg, "\n\n", response$text)

  } else {

    msg <- paste0(msg, " No JSON in response.")

  }

  stop(msg, call. = FALSE)

}
