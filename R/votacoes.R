# Recupera detalhes sobre uma votação específica pelo seu ID.
# id_vot: ID da votação.
# Return: Lista contendo os detalhes de uma votação, incluindo o posicionamento de cada partido.
fetch_votacao <- function(votacao){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", votacao, sep="")

  votacao_json <- .get_json(full_link)

  return(votacao_json$dados)
}

# Recupera os votantes referentes àquela votação específica
# id_vot: ID da votação.
# Return: Dataframe contendo o posicionamento de cada deputado, além de informações sobre estes.
fetch_votos <- function(votacao){

  id_vot <- 7252
  votantes_dataframe <- data.frame()

  for(i in 1:5){

    full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", votacao, "/votos?pagina=", i,"&itens=513", sep="")

    print(full_link)

    v <- .get_json(full_link)
    v_df <- v$dados

    votantes_dataframe <- rbind(votantes_dataframe, v_df)

  }

  return(votantes_dataframe)
}


