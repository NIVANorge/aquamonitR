suppressPackageStartupMessages(library("httr"))
suppressPackageStartupMessages(library("getPass"))
suppressPackageStartupMessages(library("jsonlite"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("dplyr"))


host <- "http://www.aquamonitor.no/"
aqua_site <- "AquaServices"
archive_site <- "AquaServices"
cache_site <- "AquaCache"


post_json <- function(token, path, in_json) {
    #' POST JSON to the AM API'
    #'
    #' Args:
    #'     token:   Str. Valid API access token
    #'     path:    Str. Path to enpoint. Will be appended to 'host'
    #'     in_json: Str. JSON to POST
    #'
    #' Returns:
    #'     Server response.
    url <- paste(host, path, sep = "")

    response <- POST(
        url,
        body = in_json,
        encode = "json",
        config = c(add_headers(Cookie = paste("aqua_key", token, sep = "=")))
        # verbose()  # Useful for debugging, but shows entire request, including password
    )

    if (response$status_code == 200) {
        return(fromJSON(content(response, "text"), flatten = TRUE))
    } else {
        report_json_error(response)
    }
}


get_json <- function(token, path) {
    #' GET JSON from the AM API'
    #'
    #' Args:
    #'     token:   Str. Valid API access token
    #'     path:    Str. Path to enpoint. Will be appended to 'host'
    #'
    #' Returns:
    #'     Server response.
    url <- paste(host, path, sep = "")

    response <- GET(
        url,
        add_headers(Cookie = paste("aqua_key", token, sep = "="))
    )

    if (response$status_code == 200) {
        return(fromJSON(content(response, "text"), flatten = TRUE))
    } else {
        report_json_error(response)
    }
}


report_json_error <- function(response) {
    #' Very basic error handling for POST and GET functions
    msg <- paste(
        "AquaMonitor failed with status:",
        response$status_code,
        "and message:",
        sep = " "
    )

    if (!is.null(response$text)) {
        msg <- paste(msg, "\n\n", response$text, sep = "")
    } else {
        msg <- paste(msg, "No JSON in response.", sep = " ")
    }

    stop(msg)
}


login <- function(username = NULL, password = NULL) {
    #' Login to Aquamonitor with your username and password. For security,
    #' avoid passing the args directly. The function will prompt for your
    #' username and password.
    #'
    #' Args:
    #'    username: Str. Optional. Aquamonitor username
    #'    password: Str. Optional. Aquamonitor password
    #'
    #' Returns:
    #'    Str. Access token valid for one day.

    if (is.null(username)) {
        username <- getPass("Username: ")
    }
    if (is.null(password)) {
        password <- getPass("Password: ")
    }

    loginurl <- paste(aqua_site, "login", sep = "/")
    loginparams <- list(username = username, password = password)
    response <- post_json(NULL, loginurl, loginparams)
    usertype <- response$Usertype

    if (!(usertype == "NoUser")) {
        token <- response$Token
    } else {
        stop("Login failed. Please check your username and password.")
    }

    return(token)
}


query <- setRefClass(
    #' Main class providing API access. Roughly equivalent to the AM-Python
    #' class here
    #'
    #'     https://github.com/NIVANorge/Aquamonitor-Python/blob/5cc5f857d45c4414011eb8cefae3cd7e945900df/AquaMonitor.py#L138
    #'
    #' although not all methods are implemented yet.
    "query",
    fields = list(
        token = "ANY",
        key = "ANY",
        table = "ANY",
        where = "ANY",
        result = "ANY"
    ),
    methods = list(
        initialize = function(where = NULL, token = NULL) {
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
                    msg <- paste(
                        "Query ended with an error:",
                        result$ErrorMessage,
                        sep = " "
                    )
                    stop(msg)
                }
            }
        },
        create_query = function() {
            query <- list()
            if (!is.null(table)) {
                query[["From"]] <- list(list(Table = table))
            }
            if (!is.null(where)) {
                query[["Where"]] <- where
            }
            url <- paste(cache_site, "/query/", sep = "")
            response <- post_json(token, url, query)

            if (is.null(response$Key)) {
                msg <- paste(
                    "Couldn't create query. Response:",
                    response,
                    sep = " "
                )
                stop(msg)
            } else {
                key <<- response$Key
            }
        },
        wait_query = function() {
            url <- paste(cache_site, "/query/", key, sep = "")
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
                        msg <- paste(
                            "Query didn't respond properly for table request:",
                            table,
                            sep = " "
                        )
                        stop(msg)
                    }
                }
            } else {
                stop("Query didn't respond properly.")
            }
        }
    )
)

get_project_chemistry <- function(proj_id, st_dt, end_dt, token = None, approved = TRUE) {
    #' Get all water chemistry data for the specified project ID and date range.
    #'
    #' Args:
    #'     proj_id:  Int.
    #'     st_dt:    Str. Start of period of interest in format 'dd.mm.yyyy'
    #'     end_dt:   Str. End of period of interest in format 'dd.mm.yyyy'
    #'     token:    Str. Optional. Valid API access token. If None, will first attempt to read
    #'               credentials from a '.auth' file in the installation folder. If this fails,
    #'               will prompt for username and password
    #'     approved: Bool. Whether to return only 'approved' samples (default) or all samples
    #'
    #' Returns:
    #'     Dataframe.

    # Query API and save result-set to cache
    where <- paste("project_id =",
        proj_id,
        "and sample_date >=",
        st_dt,
        "and sample_date <=",
        end_dt,
        sep = " "
    )

    table <- "water_chemistry_input"
    qry <- query(where = where, token = token)
    result <- qry$map(table)

    # Iterate over cache and build dataframe
    df_list <- list()
    for (page in 0:(result$Pages - 1)) {
        url <- paste(cache_site, "query", qry$key, table, page, sep = "/")
        response <- get_json(qry$token, url)
        df_list[[page + 1]] <- response$Items
    }

    df <- bind_rows(df_list)

    df <- df[!is.na(df$Value), ]

    # Tidy
    drops <- c("$type", "Id", "Sample.Id", "Method.Id")
    df <- df[, !(names(df) %in% drops)]

    df$Sample.SampleDate <- as.POSIXct(df$Sample.SampleDate)

    if (!"Sample.Depth1" %in% colnames(df)) {
        df$Sample.Depth1 <- as.numeric(NA)
    }
    if (!"Sample.Depth2" %in% colnames(df)) {
        df$Sample.Depth2 <- as.numeric(NA)
    }

    setnames(df,
        old = c(
            "Sample.SampleDate", "Sample.Station.Id", "Sample.Station.Code",
            "Sample.Station.Name", "Sample.Station.Project._Id",
            "Sample.Station.Project._Name", "Method.Name", "Method.Unit",
            "Sample.Depth1", "Sample.Depth2"
        ),
        new = c(
            "SampleDate", "StationId", "StationCode",
            "StationName", "ProjectId",
            "ProjectName", "ParameterName", "Unit",
            "Depth1", "Depth2"
        )
    )

    df <- select(
        df,
        "ProjectId",
        "ProjectName",
        "StationId",
        "StationCode",
        "StationName",
        "SampleDate",
        "Depth1",
        "Depth2",
        "ParameterName",
        "Flag",
        "Value",
        "Unit",
        "Approved",
    )

    if (approved == TRUE) {
        df <- df[which(df$Approved), ]
    }

    return(df)
}


get_projects <- function(token = NULL) {
    #' Get full list of projects in the Nivadatabase/Aquamonitor.
    #'
    #' Args:
    #'     token: Str. Optional. Valid API access token. If NULL, will prompt for
    #'            username and password
    #'
    #' Returns:
    #'     Dataframe

    if (is.null(token)) {
        token <- login()
    }
    url <- paste(aqua_site, "/api/query/Projects", sep = "")
    df <- get_json(token, url)

    # Tidy
    setnames(df,
        old = c("_Id", "_Name", "_Description", "_Number", "_StartDate", "_EndDate"),
        new = c(
            "ProjectId", "ProjectName", "Description", "ProjectCode",
            "StartDate", "EndDate"
        )
    )

    df <- select(
        df, "ProjectId", "ProjectCode", "ProjectName", "Description"
    )

    return(df)
}


get_project_stations <- function(proj_id, token) {
    #' Get stations associated with a specific project.
    #'
    #' Args:
    #'     proj_id: Int. Project ID for project of interest
    #'     token:   Str. Optional. Valid API access token. If NULL, will prompt
    #'              for username and password
    #'
    #' Returns:
    #'     Dataframe
    if (is.null(token)) {
        token <- login()
    }
    url <- paste(aqua_site, "/api/projects/", proj_id, "/stations", sep = "")
    df <- get_json(token, url)

    # Tidy
    setnames(df,
        old = c("Id", "Code", "Name", "Type._Text"),
        new = c("StationId", "StationCode", "StationName", "Type")
    )

    df <- select(
        df,
        "ProjectId",
        "StationId",
        "StationCode",
        "StationName",
        "Type"
    )

    return(df)
}