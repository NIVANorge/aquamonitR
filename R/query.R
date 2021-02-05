#' @importFrom methods new setRefClass
.query <- setRefClass(

  "query",

  fields = list("token" = "ANY", "key" = "ANY", "table" = "ANY", "where" = "ANY", "result" = "ANY"),

  methods = list(

    "initialize" = function(where = NULL, token = NULL) {

      where <<- where

      token <<- token

      key <<- NULL

      table <<- NULL

      result <<- NULL

    },

    map = function(table = NULL) {

      if (is.null(token)) {

        token <<- login()

      }

      table <<- table

      if (is.null(key)) {

        key <<- create_query()

      }

      if (!is.null(key)) {

        result <<- wait_query()

        if (is.null(result$ErrorMessage)) {

          if (is.null(table)) {

            return(result$CurrentStationIds)

          } else {

            return(result)

          }

        } else {

          msg <- paste0("Query ended with an error: ", result$ErrorMessage)

          stop(msg, call. = FALSE)

        }
      }
    },

    create_query = function() {

      query <- list()

      if (!is.null(table)) {

        query[["From"]] <- list(list("Table" = table))

      }

      if (!is.null(where)) {

        query[["Where"]] <- where

      }

      cache_site <- .get_cache_site()

      url <- paste0(cache_site, "/query/")

      response <- post_json(token, url, query)

      if (is.null(response$Key)) {

        msg <- paste0("Couldn't create query. Response: ", response)

        stop(msg, call. = FALSE)

      } else {

        key <<- response$Key

      }

    },

    wait_query = function() {

      cache_site <- .get_cache_site()

      url <- paste0(cache_site, "/query/", key)

      response <- get_json(token, url)

      if (!is.null(response$Result)) {

        while (response$Result$Ready == FALSE) {

          Sys.sleep(1)

          response <- get_json(token, url)

        }

        if (is.null(table)) {

          result <<- response$Result

        } else {

          url <- paste(url, table, sep = "/")

          response <- get_json(token, url)

          if (!is.null(response$Ready)) {

            while (response$Ready == FALSE) {

              Sys.sleep(1)

              response <- get_json(token, url)

            }

            result <<- response

          } else {

            msg <- paste0("Query didn't respond properly for table request: ", table)

            stop(msg, call. = FALSE)

          }

        }

      } else {

        stop("Query didn't respond properly.", call. = FALSE)

      }

    }

  )

)
