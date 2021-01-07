.get_host <- function() {
  
  if (nzchar(Sys.getenv("AQUAMONITOR"))) {
    host <- "http://www.aquamonitor.no/"
  } else {
    host <- "https://test-aquamonitor.niva.no/"
  }
  
  host
  
}
