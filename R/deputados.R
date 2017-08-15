# Gets deputy's details
# dep_id: Deputy's Register ID
#   ideCadastro param get from http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo/webservices/deputados/obterdeputados
# Return: List containing the details about the deputy
get_deputy <- function(dep_id){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/deputados/", dep_id, sep="")

  dep <- httr::GET(full_link)
  r <- httr::content(dep, as="text")
  dep_json <- jsonlite::fromJSON(r)
  dep_list <- dep_json$dados

  return(dep_list)

}


# Gets all deputy's expenses
# dep_id: Deputy's Register ID
#   ideCadastro param get from http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo/webservices/deputados/obterdeputados
# Return: Dataframe containing the details about the deputy's expenses
get_deputy_expenses <- function(dep_id) {

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/deputados/", dep_id, "/despesas?ordem=ASC&ordenarPor=numAno", sep="")

  dep <- httr::GET(full_link)
  r <- httr::content(dep, as="text")
  dep_json <- jsonlite::fromJSON(r)
  dep_dataframe <- dep_json$dados

  return(dep_dataframe)

}
