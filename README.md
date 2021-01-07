aquamonitR
================

Query the Nivabase directly from your R code and get results back as R
data.frames. {aquamonitR} uses the Aquamonitor API to access the
Nivabase securely, regardless of whether you’re logged in to the NIVA
network/VPN.

This repository is currently just a rough translation of the essential
parts of Roar’s
[Aquamonitor-Python](https://github.com/NIVANorge/Aquamonitor-Python)
library. The functionality of both libraries is currently limited, and
development here will lag development of Aquamonitor-Python.

## Installation

``` r
# install.packages("devtools")
remotes::install_github("NIVANorge/aquamonitR")
```

## Quick start

    library(aquamonitR)
    
    # this is to overwrite the standard host
    host <- "https://test-aquamonitor.niva.no/"
    
    # Login
    token <- login()
    
    # List all projects
    proj_df <- get_projects(token = token)
    paste0(nrow(proj_df), " projects in the database.")
    head(proj_df)
    
    # Get stations in project
    proj_id <- 12433
    stn_df <- get_project_stations(proj_id, token = token)
    paste0(nrow(stn_df), " stations in project.")
    head(stn_df)
    
    # Get water chemistry for project and time period
    st_dt <- "01.01.2019"
    end_dt <- "31.12.2019"
    df <- get_project_chemistry(proj_id, st_dt, end_dt, token = token)
    head(df)

See also the example notebook
[here](https://nbviewer.jupyter.org/github/NIVANorge/aquamonitR/blob/main/examples/query_chem.ipynb)
