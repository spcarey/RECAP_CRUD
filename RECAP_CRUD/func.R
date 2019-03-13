getCredentials <- function(serviceName)
{
  json <- Sys.getenv(serviceName)
  if (json == '')
  {
    stop("Missing VCAP_SERVICES")
    
  } else {
    
    vcapServices <- fromJSON(json, flatten = TRUE)
    
    vcapServices <- as_tibble(vcapServices$p.mysql)
    
  }
}  