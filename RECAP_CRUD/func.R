
getCredentials <- function(serviceName)
{
  json <- Sys.getenv(serviceName)
  if (json == '')
  {
    stop("Missing VCAP_SERVICES")
    
  } else {
    
    vcapServices <- jsonlite::fromJSON(json, flatten = TRUE)
    
    vcapServices <- dplyr::as_tibble(vcapServices$p.mysql)
    
  }
}  