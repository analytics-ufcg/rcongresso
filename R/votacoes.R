# Recupera detalhes sobre uma votação específica pelo seu ID.
# id_vot: ID da votação.
# Return: Lista contendo os detalhes de uma votação, incluindo o posicionamento de cada partido.
fetch_votacao <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, sep="")

  votacao_json <- .get_json(full_link)

  return(votacao_json$dados)
}

# Recupera os votantes referentes àquela votação específica
# id_vot: ID da votação.
# Return: Dataframe contendo o posicionamento de cada deputado, além de informações sobre estes.
fetch_votantes <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, "/votos?itens=513", sep="")

  votantes <- .get_json(full_link)

  return(votantes$dados)
}


