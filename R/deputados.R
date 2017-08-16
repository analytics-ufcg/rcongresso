# Recupera os detalhes de um deputado
# dep_id: ID do Deputado
#   ideCadastro param get from http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo/webservices/deputados/obterdeputados
# Return: Lista contendo detalhes sobre o deputado.
fetch_deputado <- function(dep_id){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/deputados/", dep_id, sep="")

  dep_json <- .get_json(full_link)

  return(dep_json$dados)

}


# Recupera todos os gastos de um deputado específico.
# dep_id: ID do deputado (ideCadastro)
#   ideCadastro param get from http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo/webservices/deputados/obterdeputados
# Return: Dataframe contendo detalhes sobre os gastos do deputado.
fetch_despesas_deputado <- function(dep_id) {

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/deputados/", dep_id, "/despesas?ordem=ASC&ordenarPor=numAno", sep="")

  dep_json <- .get_json(full_link)

  return(dep_json$dados)

}
