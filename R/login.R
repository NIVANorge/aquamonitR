#' @title Login to Aquamonitor
#' @description Login to Aquamonitor with your username and password.
#' @details For security, avoid passing the arguments directly. The function will prompt for your username and password.
#'
#' @param username \code{character}: Aquamonitor username (optional, see Details).
#' @param password \code{character}: Aquamonitor password (optional, see Details).
#'
#' @return \code{character}: Access token valid for one day.
#' @importFrom getPass getPass
#' @export
login <- function(username = NULL, password = NULL) {

  if (is.null(username)) {
    username <- getPass::getPass("Username: ")
    username <- enc2utf8(username)
  }
  if (is.null(password)) {
    password <- getPass::getPass("Password: ")
    password <- enc2utf8(password)
  }

  aqua_site <- .get_aqua_site()

  loginurl <- paste(aqua_site, "login", sep = "/")
  loginparams <- list(username = username, password = password)

  response <- .post_json(NULL, loginurl, loginparams)

  usertype <- response$Usertype

  if (!(usertype == "NoUser")) {
    token <- response$Token
  } else {
    stop("Login failed. Please check your username and password.", call. = FALSE)
  }

  token

}
