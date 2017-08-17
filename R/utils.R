.get_json <- function(full_link){

  prop <- httr::GET(full_link)
  r <- httr::content(prop, as="text")
  prop_json <- jsonlite::fromJSON(r, flatten = T)

  return(prop_json)
}
